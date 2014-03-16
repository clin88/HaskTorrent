{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
module Peers
    --( PeerAddr (..)
    --, Peer
    --, connectToPeer)
where

import Control.Exception (handle, IOException)
import Control.Applicative ((<$>), (<*>))
import Data.Either (rights)
import Data.Maybe (fromMaybe)
import Control.Exception (try)
import Control.Concurrent.Async (mapConcurrently)
import Data.Binary (Binary, get, put, decode, encode)
import qualified Data.Binary                as BIN
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Word
import           Network
import           System.IO
import Network (PortNumber (..))

data PeerAddr = PeerAddr
    { paIp   :: HostName
    , paPort :: PortID
    , paId :: Maybe ByteString } deriving (Show)

data Peer = Peer
    { peerIp :: HostName
    , peerPort :: PortID
    , peerId :: ByteString
    , peerHandle :: Handle } deriving (Show)
    -- haveField
    -- choked/interested flags

data Handshake = Handshake
    { hsProtocolId     :: ByteString
    , hsProtocolParams :: Word64
    , hsInfoHash       :: ByteString
    , hsPeerId         :: ByteString } deriving (Show)

instance Binary Handshake where
    get = do
        pstrlen <- get :: Get Word8
        protid <- getByteString (fromIntegral pstrlen)
        pparams <- get :: Get Word64
        infoh <- getByteString 20
        peerid  <- getByteString 20
        return $ Handshake protid pparams infoh peerid

    put Handshake {..} = do
        put pstrlen
        putByteString hsProtocolId
        put hsProtocolParams
        putByteString hsInfoHash
        putByteString hsPeerId
        where
            pstrlen :: Word8
            pstrlen = fromIntegral $ B.length hsProtocolId

data PeerMessage =
      KeepAlive
    | Choke
    | Unchoke
    | Interested
    | Uninterested
    | Have { haveIndex :: Int }
    | Bitfield { bfBitfield :: ByteString}
    | Request
        { reqIndex :: Int
        , reqBegin :: Int
        , reqLength :: Int }
    | Piece
        { pieceIndex :: Int
        , pieceBegin :: Int
        , pieceBlock :: ByteString }
    | Cancel
        { cancIndex :: Int
        , cancBegin :: Int
        , cancLength :: Int }
    | Port
        { portPort :: PortID }

instance Binary PeerMessage where
    get = do
        len <- getNum32
        msgid <- lookAheadM getId
        case msgid of
            Nothing -> return KeepAlive
            Just 0  -> return Choke
            Just 1  -> return Unchoke
            Just 2  -> return Interested
            Just 3  -> return Uninterested
            Just 4  -> Have <$> getNum32
            Just 5  -> Bitfield <$> (getByteString $ len - 1)
            Just 6  -> Request <$> getNum32 <*> getNum32 <*> getNum32
            Just 7  -> Piece <$> getNum32 <*> getNum32 <*> (getByteString $ len - 9)
            Just 8  -> Cancel <$> getNum32 <*> getNum32 <*> getNum32
            Just 9  -> Port <$> PortNumber . fromIntegral <$> (get :: Get Word16)

        where
            getId = do
                empty <- isEmpty
                if empty then return Nothing
                         else getWord8 >>= return . Just
            getNum32 = fromIntegral <$> (get :: Get Word32)

    put KeepAlive = putWord8 0
    put Choke = putWord32 1 >> putWord8 0
    put Unchoke = putWord32 1 >> putWord8 1
    put Interested = putWord32 1 >> putWord8 2
    put Uninterested = putWord32 1 >> putWord8 3
    put (Have index) = putWord32 5 >> putWord8 4 >> putWord32 index
    put (Bitfield bf) = putWord8 0
    put Request {..} = do
        putWord32 13
        putWord8 6
        putWord32 reqIndex
        putWord32 reqBegin
        putWord32 reqLength
    put Piece {..} = do
        putWord32 $ 9 + B.length pieceBlock
        putWord8 7
        putWord32 pieceIndex
        putWord32 pieceBegin
        putByteString pieceBlock
    put Cancel {..} = do
        putWord32 13
        putWord8 8
        putWord32 cancIndex
        putWord32 cancBegin
        putWord32 cancLength
    put (Port (PortNumber port)) = do
        putWord32 3
        putWord8 9
        put (fromIntegral port :: Word16)

putWord32 :: Integral a => a -> Put
putWord32 = put . (fromIntegral :: Integral a => a -> Word32)

formHandshake :: ByteString -> ByteString -> Handshake
formHandshake infohash peerid = Handshake "BitTorrent protocol" 0 infohash peerid

-- Connects to peer and launches peer threads. Does not close handle automatically,
-- so operation should be bracketed.
connectToPeer :: Handshake -> ByteString -> PeerAddr -> IO (Either String Peer)
connectToPeer handshake infohash (PeerAddr {..}) = do
    handle <- connectTo paIp paPort
    hSetBinaryMode handle True
    handshakeToPeer handle handshake
    Handshake {..} <- handshakeFromPeer handle

    -- checks to validate peer
    if | hsProtocolId /= "BitTorrent protocol" -> return $ Left "Peer not using 'BitTorrent protocol'."
       | hsInfoHash /= infohash -> return $ Left "Peer seeking handshake for wrong file."
       | fromMaybe hsPeerId paId == hsPeerId -> return $ Left "Peer ID doesn't match that reported by tracker."
       | otherwise -> return $ Right $ Peer
            { peerIp = paIp
            , peerPort = paPort
            , peerId = hsPeerId
            , peerHandle = handle }

--connectToPeers :: Handshake -> ByteString -> [PeerAddr] -> IO [Peer]
connectToPeers hs infoh peers = mapConcurrently (handle hdl . connectToPeer hs infoh) peers
    where
        hdl :: IOException -> IO (Either String a)
        hdl _ = return $ Left "IO exception occurred."

handshakeToPeer :: Handle -> Handshake -> IO ()
handshakeToPeer handle handshake = L.hPut handle $ BIN.encode handshake

handshakeFromPeer :: Handle -> IO Handshake
handshakeFromPeer handle = do
    pstrlen <- L.hGet handle 1
    let intPstrlen = fromIntegral (decode pstrlen :: Word8)
    peerhs <- L.hGet handle $ intPstrlen + 49
    return $ BIN.decode $ pstrlen `L.append` peerhs
