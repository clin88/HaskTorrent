{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Peers
    --( PeerAddr (..)
    --, Peer
    --, connectToPeer)
where

import Text.Printf (printf)
import           Control.Concurrent.STM
import           Control.Monad          (forM_, forever, join)
import qualified Data.Binary            as BIN
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as L
import           Data.IntMap.Strict     (IntMap)
import qualified Data.IntMap.Strict     as IM
import           Data.Maybe             (fromMaybe)
import           Data.Monoid            ((<>))
import           Data.Sequence          (Seq, (|>))
import qualified Data.Sequence          as Seq
import           Data.Set               (Set)
import qualified Data.Set               as S
import           Data.Word
import           Network                (HostName, PortID (..), connectTo)
import           PeerMsgs
import           System.IO              (Handle, hSetBinaryMode)

data PeerAddr = PeerAddr
    { peerHost      :: HostName
    , peerPort      :: PortID
    , peerTrackerId :: Maybe ByteString } deriving (Show)

data PieceSt = Claimed
             | Downloaded
             | Unclaimed
             deriving (Show)

type PiecesMap = IntMap PieceSt
type PieceID = Int

data Peer = Peer
    { pAmChoking    :: Bool
    , pAmInterested :: Bool
    , pChokingMe    :: Bool
    , pInterestedMe :: Bool
    , pHasPieces    :: Set Int
    , pHaveMapGlob  :: PiecesMap
    , pReqsFrom     :: Seq PeerRequest
    , pReqsTo       :: Seq PeerRequest
    } deriving (Show)

defaultPeer :: PiecesMap -> Peer
defaultPeer globalhasmap = Peer
    { pAmChoking = True
    , pAmInterested = False
    , pChokingMe = True
    , pInterestedMe = False
    , pHasPieces = S.empty
    , pHaveMapGlob  = globalhasmap
    , pReqsFrom = Seq.empty
    , pReqsTo = Seq.empty }

-- note to self:
--  Interest is updated at two points-when a new piece comes in (may lose interest)
--  or when new bitfield or have information comes in (may gain interest)

sendMsg :: Handle -> PeerMessage -> IO ()
sendMsg to msg = do
    printf "SENDING MSG: %s\n" (show msg)
    B.hPut to $ encodeMsg msg

peerController :: TVar Peer -> TVar PiecesMap -> Handle -> IO ()
peerController peer glHaves handle = forever . join . atomically $
    sendHaves peer glHaves handle
    -- make a request
    -- if not choked
    -- and they're interested
    -- and I have this piece
    -- and

    --TODO: Handle request prioritization into just currently claimed pieces.

    --if unchoked ...
    --    if not interested and have interest, express interest, get requests
    --    elif interested but have no interest,
    --    if pending requests are old, delete them
    --    if pending requests < 5 and interested, send request            -- can we guarantee we'll never be interested when there's nothing we need?bou


-- peerController actions
sendHaves :: TVar Peer -> TVar PiecesMap -> Handle -> STM (IO ())
sendHaves peer glHaves handle = do
    Peer {..} <- readTVar peer
    pHaveMapGlob' <- readTVar glHaves
    case findHaves pHaveMapGlob' pHaveMapGlob  of
        [] -> retry
        xs -> return $ do
            printf "SENDING HAVES: %s\n" (show xs)
            forM_ xs (sendMsg handle . Have)
    where
        findHaves :: PiecesMap -> PiecesMap -> [PieceID]
        findHaves new old = IM.keys $ IM.differenceWith diff new old

        diff :: PieceSt -> PieceSt -> Maybe PieceSt
        diff d@Downloaded Claimed = return d
        diff d@Downloaded Unclaimed = return d
        diff _ _ = Nothing

-- Listens to incoming messages and changes state/responds to message.
peerListener :: TVar Peer -> Handle -> IO ()
peerListener peer handle = forever $ do
    msg <- getMsg
    printf "LISTENER: %s\n" (show msg)
    case msg of
        KeepAlive        -> return ()
        Choke            -> updatePeer (\p -> p { pChokingMe = True })
        Unchoke          -> updatePeer (\p -> p { pChokingMe = False })
        Interested       -> updatePeer (\p -> p { pInterestedMe = True })
        Uninterested     -> updatePeer (\p -> p { pInterestedMe = False })
        -- TODO: add gain interest test code here
        Bitfield ps      -> updatePeer (\p -> p { pHasPieces = ps })
        Have ind         -> updatePeer (\p@(Peer {..}) -> p { pHasPieces = S.insert ind pHasPieces })
        Request req      -> updatePeer (\p@(Peer {..}) -> p { pReqsFrom = pReqsFrom |> req } )
        -- TODO: Handle piece adding.
        piece@Piece {..} -> print piece
        Cancel req       -> updatePeer (\p@(Peer {..}) -> p { pReqsFrom = Seq.filter (req /=) pReqsFrom })
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
peerHandshake :: Handle -> ByteString -> ByteString -> Maybe ByteString -> IO ()
peerHandshake handle infohash peerid trackerid = do
    handshakeToPeer handle $ formHandshake infohash peerid
    peerhs@Handshake {..} <- handshakeFromPeer handle
    case validateHandshake peerhs (fromMaybe hsPeerId trackerid) infohash of
        Left s    -> fail $ "invalid handshake: " ++ s
        _         -> return ()

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
