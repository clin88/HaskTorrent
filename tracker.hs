{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ViewPatterns #-}

module Tracker where
--    ( makeRequest
--    , BTTrackerRequest (..)
--    , BTTrackerResponse (..)
--    , getTrackerRequest) where

import Data.Typeable (Typeable)
import           Control.Applicative   ((<|>))
import           Data.BEncode
import qualified Data.ByteString.Char8 as BS8 (pack, unpack)
import qualified Data.ByteString as BS (ByteString, unpack)
import           Metainfo              (BTMetainfo (..), infoHash, totalSize,
                                        trackers)
import           Network.HTTP          (getRequest, getResponseBody, simpleHTTP)
import           Network.HTTP.Types    (renderSimpleQuery)
import Data.Word (Word8, Word16)
import Data.List.Split (chunksOf)

{- DATA TYPES -}

data BTTrackerRequest = BTTrackerRequest
    { info_hash  :: BS.ByteString
    , peer_id    :: BS.ByteString
    , port       :: Int
    , uploaded   :: Int
    , downloaded :: Int
    , left       :: Int
    , compact    :: NSBool
    , no_peer_id :: NSBool
    , event      :: BTEvents } deriving (Show)

data BTEvents = Started | Stopped | Completed
instance Show BTEvents where
    show Started = "started"
    show Stopped = "stopped"
    show Completed = "completed"

newtype NSBool = NSBool { unNSBool :: Bool } deriving (Eq)
instance Show NSBool where
    show (NSBool True) = "1"
    show (NSBool False) = "0"

-- Represents a response from the tracker when requesting announce.
data BTTrackerResponse =
    BTTrackerFailure
    { failureReason :: BS.ByteString } |
    BTTrackerResponse
    { complete       :: Int
    , incomplete     :: Int
    , interval       :: Int
    , minInterval    :: Maybe Int
    , peers          :: Peers
    , trackerId      :: Maybe Int
    , warningMessage :: Maybe BS.ByteString }
    deriving (Show, Typeable)

instance BEncode BTTrackerResponse where
    toBEncode = error "Encoding for BTTrackerResult not implemented."
    fromBEncode dct = failure dct <|> success dct
        where
            failure = fromDict $ BTTrackerFailure
                <$>! "failure reason"
            success = fromDict $ BTTrackerResponse
                <$>! "complete"
                <*>! "incomplete"
                <*>! "interval"
                <*>? "min interval"
                <*>! "peers"
                <*>? "tracker id"
                <*>? "warning message"

data Peer = Peer
    { peerIp   :: [Word8]
    , peerPort :: Word16 } deriving (Show)

newtype Peers = Peers [Peer] deriving (Show)
instance BEncode Peers where
    toBEncode = error "Encoding of peer list not implemented."
    fromBEncode (BString val) = do return $ procPeerList val

{- MAKE REQUEST -}

getTrackerRequest :: BTMetainfo -> BTTrackerRequest
getTrackerRequest minfo@(BTMetainfo {..}) = BTTrackerRequest
    { info_hash = infoHash info
    , peer_id = "HT123456789012345678"           -- TODO: replace with UUID
    , port = 6881
    , uploaded = 0
    , downloaded = 0
    , left = totalSize minfo
    , compact = NSBool True
    , no_peer_id = NSBool True
    , event = Started }

makeQueryString :: BTTrackerRequest -> String
makeQueryString BTTrackerRequest {..} = BS8.unpack $ renderSimpleQuery True $
    [ ("info_hash", info_hash)
    , ("peer_id", peer_id)
    , ("left", castBS left) ]
    where
        castBS :: (Show a) => a -> BS.ByteString
        castBS = BS8.pack . show

makeRequest :: BS.ByteString -> BTTrackerRequest -> IO String
makeRequest url req = do
    response <- simpleHTTP request
    getResponseBody response
    where
        urlString = BS8.unpack url
        request = getRequest $ urlString ++ makeQueryString req

{- PARSE RESPONSE -}

procPeerList :: BS.ByteString -> Peers
procPeerList = Peers . map procPeer . chunksOf 6 . BS.unpack

procPeer :: [Word8] -> Peer
procPeer rawPeer = Peer ip (x*256 + y)
    where
        (ip, ports) = splitAt 4 rawPeer
        (x:y:[]) = (map fromIntegral ports :: [Word16])
