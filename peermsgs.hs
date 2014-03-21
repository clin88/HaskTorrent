{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module PeerMsgs
    ( Handshake(..)
    , formHandshake
    , PeerMessage(..)
    , Block(..)
    , encodeMsg
    , decodeMsg
    , encodeHandshake
    , decodeHandshake )
    where

import           Data.Binary                (Binary, decode, encode, get, put)
import           Data.ByteString            (ByteString)
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Sequence              (Seq)
import           Data.Word

import qualified Data.Binary                as Bin
import qualified Data.Bits                  as Bits
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as L
import qualified Data.Sequence as Seq

import           Control.Applicative        ((<$>), (<*>))

import           Network

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

formHandshake :: ByteString -> ByteString -> Handshake
formHandshake infohash peerid = Handshake "BitTorrent protocol" 0 infohash peerid

data Block = Block
    { reqIndex  :: Int
    , reqBegin  :: Int
    , reqLength :: Int } deriving (Show, Eq, Ord)

data PeerMessage =
      KeepAlive
    | Choke
    | Unchoke
    | Interested
    | Uninterested
    | Have Int
    | Bitfield (Seq Bool)
    | Request Block
    | BlockMsg
        { blockIndex :: Int
        , blockBegin :: Int
        , blockContent :: ByteString }
    | Cancel Block
    | Port { portPort :: PortID } deriving (Show, Eq)

instance Binary PeerMessage where
    get = do
        len <- getNum32
        msgid <- getId
        case msgid of
            Nothing -> return KeepAlive
            Just 0  -> return Choke
            Just 1  -> return Unchoke
            Just 2  -> return Interested
            Just 3  -> return Uninterested
            Just 4  -> Have <$> getNum32
            Just 5  -> Bitfield <$> (decodeBitField <$> (getByteString $ len - 1))
            Just 6  -> Request <$> (Block <$> getNum32 <*> getNum32 <*> getNum32)
            Just 7  -> BlockMsg <$> getNum32 <*> getNum32 <*> (getByteString $ len - 9)
            Just 8  -> Cancel <$> (Block <$> getNum32 <*> getNum32 <*> getNum32)
            Just 9  -> Port <$> PortNumber . fromIntegral <$> (get :: Get Word16)
        where
            getId = do
                empty <- isEmpty
                if empty then return Nothing
                         else Just <$> getWord8
            getNum32 = fromIntegral <$> (get :: Get Word32)

    put KeepAlive = putWord32 0
    put Choke = putWord32 1 >> putWord8 0
    put Unchoke = putWord32 1 >> putWord8 1
    put Interested = putWord32 1 >> putWord8 2
    put Uninterested = putWord32 1 >> putWord8 3
    put (Have index) = putWord32 5 >> putWord8 4 >> putWord32 index
    put (Bitfield bf) = do
        let blen = ceiling $ fromIntegral (Seq.length bf) / 8
        putWord32 $ blen + 1
        putWord8 5
        putByteString $ encodeBitField bf
    put (Request (Block {..})) = do
        putWord32 13
        putWord8 6
        putWord32 reqIndex
        putWord32 reqBegin
        putWord32 reqLength
    put BlockMsg {..} = do
        putWord32 $ 9 + B.length blockContent
        putWord8 7
        putWord32 blockIndex
        putWord32 blockBegin
        putByteString blockContent
    put (Cancel (Block {..})) = do
        putWord32 13
        putWord8 8
        putWord32 reqIndex
        putWord32 reqBegin
        putWord32 reqLength
    put (Port (PortNumber port)) = do
        putWord32 3
        putWord8 9
        put (fromIntegral port :: Word16)

{- SUPPORT FUNCTIONS -}

encodeBitField :: Seq Bool -> ByteString
encodeBitField = B.pack . reverse . Seq.foldlWithIndex combine []
    where
        combine xs ind bool =
            let bitind = ind `mod` 8
                bbit word = if bool then Bits.setBit word bitind
                                    else word
            in  case bitind of 0 -> bbit 0 : xs
                               _ -> bbit (head xs) : tail xs

decodeBitField :: ByteString -> Seq Bool
decodeBitField f = Seq.fromList $ Bits.testBit <$> B.unpack f <*> [0..7]

putWord32 :: Integral a => a -> Put
putWord32 = put . (fromIntegral :: Integral a => a -> Word32)

{- EXPORT FUNCTIONS -}

encodeMsg :: PeerMessage -> ByteString
encodeMsg = L.toStrict . Bin.encode

decodeMsg :: ByteString -> PeerMessage
decodeMsg = Bin.decode . L.fromStrict

encodeHandshake :: Handshake -> ByteString
encodeHandshake = L.toStrict . Bin.encode

decodeHandshake :: ByteString -> Handshake
decodeHandshake = Bin.decode . L.fromStrict
