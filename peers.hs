{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Peers
    --( PeerAddr (..)
    --, Peer
    --, connectToPeer)
where

import Prelude hiding (sequence)
import
import           Control.Applicative      ((<$>), (<*>))
import           Control.Concurrent.Async (mapConcurrently)
import           Control.Concurrent.STM
import           Control.Exception        (IOException, handle, try)
import qualified Data.Binary as BIN
import           Data.Monoid           ((<>))
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as B
import qualified Data.ByteString.Lazy     as L
import           Data.Maybe               (fromMaybe)
import qualified Data.Sequence            as Seq
import           Data.Sequence            (Seq, (|>))
import           Data.Set                 (Set)
import qualified Data.Set                 as S
import           Data.Word
import           Network                  (HostName, PortID (..),
                                           PortNumber (..), connectTo)
import           PeerMsgs
import           System.IO                (Handle, hSetBinaryMode)

data PeerAddr = PeerAddr
    { peerHost      :: HostName
    , peerPort      :: PortID
    , peerTrackerId :: Maybe ByteString } deriving (Show)

data Peer = Peer
    { peerAmChoking    :: Bool
    , peerAmInterested :: Bool
    , peerChokingMe    :: Bool
    , peerInterestedMe :: Bool
    , peerHasMap       :: HaveMap
    , peerLocalHasMap  :: HaveMap
    , peerReqsFrom     :: Seq PeerRequest
    , peerReqsTo       :: Seq PeerRequest
    } deriving (Show)

type HaveMap = Set Int

-- note to self:
--  Interest is updated at two points-when a new piece comes in (may lose interest)
--  or when new bitfield or have information comes in (may gain interest)
--
--
sendMsg (encodeMsg -> msg) = B.hPut handle msg


peerController :: TVar Peer -> TVar HaveMap -> Handle -> IO ()
peerController peer haves handle = forever . join . atomically $ do
    processMsg <> stm() <>

data PieceSt = Claimed | Downloaded | Unclaimed
type PieceInfo = (Int, PieceSt)
Data.Map Int PieceSt

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
        sendHave haves haveimage = case haves \\ haveimage of
                S.empty -> retry
                diff    -> return traverse sendMsg diff

-- Listens to incoming messages and changes state/responds to message.
peerListener :: TVar Peer -> Handle -> IO ()
peerListener peer handle = forever $ do
    msg <- getMsg
    case msg of
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
            return . decodeMsg $ len <> msg

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
        Left s      -> fail $ "Invalid handshake: " ++ s
        otherwise   -> return $ Peer
            { peerAmChoking = True
            , peerAmInterested = False
            , peerChokingMe = True
            , peerInterestedMe = False
            , peerHasMap = S.empty
            , peerReqsFrom = Seq.empty }

handshakeToPeer :: Handle -> Handshake -> IO ()
handshakeToPeer handle handshake = L.hPut handle $ encodeHandshake handshake

handshakeFromPeer :: Handle -> IO Handshake
handshakeFromPeer handle = do
    pstrlen <- L.hGet handle 1
    let intPstrlen = fromIntegral $ B.head pstrlen
    peerhs <- L.hGet handle $ intPstrlen + 49
    return . decodeHandshake $ pstrlen <> peerhs

validateHandshake :: Handshake -> ByteString -> ByteString -> Either String ()
validateHandshake (Handshake {..}) expectedPeerId infohash
    | hsProtocolId /= "BitTorrent protocol" = Left "Peer not using 'BitTorrent protocol'."
    | hsInfoHash /= infohash                = Left "Peer seeking handshake for wrong file."
    | hsPeerId /= expectedPeerId            = Left "Peer ID doesn't match that reported by tracker."
    | otherwise                             = Right ()
