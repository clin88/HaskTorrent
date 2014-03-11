{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Tracker where
--    ( makeRequest
--    , BTTrackerRequest (..)
--    , BTTrackerResponse (..)
--    , makeRequestObject) where

import           Control.Applicative      ((<|>))
import           Control.Concurrent.Async (mapConcurrently)
import           Control.Exception        (IOException, try)
import           Data.BEncode
import qualified Data.ByteString          as BS (ByteString, take, unpack)
import qualified Data.ByteString.Char8    as BS8 (pack, unpack)
import           Data.Either              (rights)
import           Data.List                (intersperse)
import           Data.List.Split          (chunksOf)
import           Data.Typeable            (Typeable)
import           Data.Word                (Word16, Word8)
import           Metainfo                 (BTMetainfo (..), infoHash, totalSize,
                                           trackers)
import           Network                  (HostName, PortID (..),
                                           PortNumber (..))
import           Network.HTTP             (getRequest, getResponseBody,
                                           simpleHTTP)
import           Network.HTTP.Types       (renderSimpleQuery)

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
    { peerIp   :: HostName
    , peerPort :: PortID } deriving (Show)

newtype Peers = Peers { unPeers :: [Peer] } deriving (Show)
instance BEncode Peers where
    toBEncode = error "Encoding of peer list not implemented."
    fromBEncode (BString val) = do return $ procPeerList val

{- MAKE REQUEST -}

-- Concurrently shoot a request to all trackers in a metainfo file.
--
-- TODO: Add timeouts to each request so it doesn't take forever for bad requests.
announceAllTrackers :: BTMetainfo -> IO [BTTrackerResponse]
announceAllTrackers minfo = do
    results <- mapConcurrently safeRequest $ trackers minfo
    return $ rights results
    where
        req = makeRequestObject minfo
        safeRequest :: BS.ByteString -> IO (Either IOException BTTrackerResponse)
        safeRequest = try . makeRequest req

-- TODO: Fix errors here to be catchable under one umbrella
makeRequest :: BTTrackerRequest -> BS.ByteString -> IO BTTrackerResponse
makeRequest req url
    | urlHead /= "http" = fail "URL not HTTP."
    | otherwise = do
        response <- simpleHTTP request
        body <- fmap BS8.pack $ getResponseBody response
        return $ either error id $ (decode body :: Result BTTrackerResponse)
    where
        urlHead = BS.take 4 url
        urlString = BS8.unpack url
        request = getRequest $ urlString ++ makeQueryString req

makeRequestObject :: BTMetainfo -> BTTrackerRequest
makeRequestObject minfo@(BTMetainfo {..}) = BTTrackerRequest
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

{- PARSE RESPONSE -}
-- helper function to create return all peers from a list of responses
extractAllPeers :: [BTTrackerResponse] -> [Peer]
extractAllPeers = concatMap (unPeers . peers)

procPeerList :: BS.ByteString -> Peers
procPeerList = Peers . map procPeer . chunksOf 6 . BS.unpack

procPeer :: [Word8] -> Peer
procPeer rawPeer = Peer host $ PortNumber (x*256 + y)
    where
        (ip, ports) = splitAt 4 rawPeer
        host = concat $ intersperse "." $ map show ip
        (x:y:[]) = (map fromIntegral ports :: [PortNumber])
