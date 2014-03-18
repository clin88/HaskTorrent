{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Peers
    --( PeerAddr (..)
    --, Peer
    --, connectToPeer)
where

import Control.Monad (forever, join, forM_)
import qualified Data.IntMap.Strict as IM
import           Data.IntMap.Strict (IntMap)
-- import           Control.Applicative      ((<$>), (<*>))
-- import           Control.Concurrent.Async (mapConcurrently)
import           Control.Concurrent.STM
-- import           Control.Exception        (IOException, handle, try)
import qualified Data.Binary as BIN
import           Data.Monoid           ((<>))
import           Data.ByteString          (ByteString)
-- import Text.Printf (printf)
import qualified Data.ByteString          as B
import qualified Data.ByteString.Lazy     as L
import           Data.Maybe               (fromMaybe)
import qualified Data.Sequence            as Seq
import           Data.Sequence            (Seq, (|>))
import           Data.Set                 (Set)
import qualified Data.Set                 as S
import           Data.Word
import           Network                  (HostName, PortID (..), connectTo)
import           PeerMsgs
import           System.IO                (Handle, hSetBinaryMode)

data PeerAddr = PeerAddr
    { peerHost      :: HostName
    , peerPort      :: PortID
    , peerTrackerId :: Maybe ByteString } deriving (Show)

data PieceSt = Claimed
             | Downloaded
             | Unclaimed
             deriving (Show)

type HaveMap = IntMap PieceSt

data Peer = Peer
    { peerAmChoking    :: Bool
    , peerAmInterested :: Bool
    , peerChokingMe    :: Bool
    , peerInterestedMe :: Bool
    , peerHasMap       :: Set Int
    , peerHasMapGlobal :: HaveMap
    , peerReqsFrom     :: Seq PeerRequest
    , peerReqsTo       :: Seq PeerRequest
    } deriving (Show)

type PieceID = Int

-- note to self:
--  Interest is updated at two points-when a new piece comes in (may lose interest)
--  or when new bitfield or have information comes in (may gain interest)
--
--

sendMsg :: Handle -> PeerMessage -> IO ()
sendMsg to = B.hPut to . encodeMsg
-- sendMsg (encodeMsg -> msg) = B.hPut handle msg

peerController :: TVar Peer -> TVar HaveMap -> Handle -> IO ()
peerController peer haves handle = forever . join . atomically
    $ sendHaves `orElse` patrick
    -- make a request
    -- if not choked
    -- and they're interested
    -- and I have this piece
    -- and
    {-
    TODO: Handle request prioritization into just currently claimed pieces.

    if unchoked ...
        if not interested and have interest, express interest, get requests
        elif interested but have no interest,
        if pending requests are old, delete them
        if pending requests < 5 and interested, send request            -- can we guarantee we'll never be interested when there's nothing we need?bou
    -}
    where
        sendHaves :: STM (IO ())
        sendHaves = do
            Peer {..} <- readTVar peer
            peerHasMapGlobal' <- readTVar haves
            case findHaves peerHasMapGlobal' peerHasMapGlobal of
                [] -> retry
                xs -> return $ forM_ xs (sendMsg handle . Have)
        patrick :: STM (IO ())
        patrick = do
            return $ print ("cats" :: String)

findHaves :: HaveMap -> HaveMap -> [PieceID]
findHaves new old = IM.keys $ IM.differenceWith diff new old
    where
        diff :: PieceSt -> PieceSt -> Maybe PieceSt
        diff d@Downloaded Claimed = return d
        diff d@Downloaded Unclaimed = return d
        diff _ _ = Nothing

-- Listens to incoming messages and changes state/responds to message.
peerListener :: TVar Peer -> Handle -> IO ()
peerListener peer handle = forever $ do
    msg <- getMsg
    case msg of
        KeepAlive        -> return ()
        Choke            -> updatePeer (\p -> p { peerChokingMe = True })
        Unchoke          -> updatePeer (\p -> p { peerChokingMe = False })
        Interested       -> updatePeer (\p -> p { peerInterestedMe = True })
        Uninterested     -> updatePeer (\p -> p { peerInterestedMe = False })
        -- TODO: add gain interest test code here
        Bitfield ps      -> updatePeer (\p -> p { peerHasMap = ps })
        Have ind         -> updatePeer (\p@(Peer {..}) -> p { peerHasMap = S.insert ind peerHasMap })
        Request req      -> updatePeer (\p@(Peer {..}) -> p { peerReqsFrom = peerReqsFrom |> req } )
        -- TODO: Handle piece adding.
        piece@Piece {..} -> print piece
        Cancel req       -> updatePeer (\p@(Peer {..}) -> p { peerReqsFrom = Seq.filter (req /=) peerReqsFrom })
        Port p           -> return ()
    where
        updatePeer = atomically . modifyTVar peer
        getMsg = do
            len <- L.hGet handle 4
            let intLen = fromIntegral (BIN.decode len :: Word32)
            msg <- L.hGet handle intLen
            return . decodeMsg . L.toStrict $ len <> msg

-- Connects to peer and launches peer threads. Does not close handle automatically,
-- so operation should be bracketed.
connectToPeer :: Handshake -> ByteString -> PeerAddr -> IO Peer
connectToPeer handshake infohash (PeerAddr {..}) = do
    handle <- connectTo peerHost peerPort
    hSetBinaryMode handle True
    handshakeToPeer handle handshake
    peerhs@Handshake {..} <- handshakeFromPeer handle
    let trackerId = fromMaybe hsPeerId peerTrackerId
    case validateHandshake peerhs trackerId infohash of
        Left s    -> fail $ "Invalid handshake: " ++ s
        _         -> return $ Peer
            { peerAmChoking = True
            , peerAmInterested = False
            , peerChokingMe = True
            , peerInterestedMe = False
            , peerHasMap = S.empty
            , peerHasMapGlobal = IM.empty
            -- move this instatiation to a smart constructor for Peer that
            -- takes the inital global hasmap
            , peerReqsFrom = Seq.empty }

handshakeToPeer :: Handle -> Handshake -> IO ()
handshakeToPeer handle handshake = B.hPut handle $ encodeHandshake handshake

handshakeFromPeer :: Handle -> IO Handshake
handshakeFromPeer handle = do
    pstrlen <- L.hGet handle 1
    let intPstrlen = fromIntegral $ L.head pstrlen
    peerhs <- L.hGet handle $ intPstrlen + 49
    return . decodeHandshake . L.toStrict $ pstrlen <> peerhs

validateHandshake :: Handshake -> ByteString -> ByteString -> Either String ()
validateHandshake (Handshake {..}) expectedPeerId infohash
    | hsProtocolId /= "BitTorrent protocol" = Left "Peer not using 'BitTorrent protocol'."
    | hsInfoHash /= infohash                = Left "Peer seeking handshake for wrong file."
    | hsPeerId /= expectedPeerId            = Left "Peer ID doesn't match that reported by tracker."
    | otherwise                             = Right ()
