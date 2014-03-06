{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Tracker
    ( makeRequest
    , BTTrackerRequest (..)
    , getTrackerRequest
    , BTTrackerResponse (..) ) where

import Data.Typeable (Typeable)
import           Control.Applicative   ((<|>))
import           Data.BEncode
import qualified Data.ByteString.Char8 as BS8 (ByteString, pack, unpack)
import qualified Data.ByteString as BS (unpack)
import           Metainfo              (BTMetainfo (..), infoHash, totalSize,
                                        trackers)
import           Network.HTTP          (getRequest, getResponseBody, simpleHTTP)
import           Network.HTTP.Types    (renderSimpleQuery)
import Data.Word (Word8, Word16)

{- DATA TYPES -}

-- Represents a response from the tracker when requesting announce.
data BTTrackerResponse =
    BTTrackerFailure
    { failureReason :: BS8.ByteString } |
    BTTrackerResponse
    { complete       :: Int
    , incomplete     :: Int
    , interval       :: Int
    , minInterval    :: Maybe Int
    , peers          :: Peers
    , trackerId      :: Maybe Int
    , warningMessage :: Maybe BS8.ByteString }
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
    { pIp   :: [Word8]
    , pPort :: [Word8] } deriving (Show)

newtype Peers = Peers [Peer] deriving (Show)
instance BEncode Peers where
    toBEncode = error "Encoding of peer list not implemented."
    fromBEncode val = do return $ getPeers val

getPeers :: BValue -> Peers
getPeers (BString bs) = Peers $ getPeers' $ BS.unpack bs

getPeers' :: [Word8] -> [Peer]
getPeers' [] = []
getPeers' xs = Peer { pIp=ip, pPort=port }:getPeers' tail
    where
        (sixdigs, tail) = splitAt 6 xs
        (ip, port) = splitAt 4 sixdigs

data BTTrackerRequest = BTTrackerRequest
    { info_hash  :: BS8.ByteString
    , peer_id    :: BS8.ByteString
    , port       :: Int
    , uploaded   :: Int
    , downloaded :: Int
    , left       :: Int
    , compact    :: NSBool
    , no_peer_id :: NSBool
    , event      :: BTEvents } deriving (Show)

data BTEvents = Started | Stopped | Completed
newtype NSBool = NSBool { unNSBool :: Bool } deriving (Eq)

instance Show NSBool where
    show (NSBool True) = "1"
    show (NSBool False) = "0"

instance Show BTEvents where
    show Started = "started"
    show Stopped = "stopped"
    show Completed = "completed"

type TrackerURL = String
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

{- LOWER LEVEL IMPLEMENTATION -}

makeQueryString :: BTTrackerRequest -> String
makeQueryString BTTrackerRequest {..} = BS8.unpack $ renderSimpleQuery True $
    [ ("info_hash", info_hash)
    , ("peer_id", peer_id)
    , ("left", castBS left) ]
    where
        castBS :: (Show a) => a -> BS8.ByteString
        castBS = BS8.pack . show

makeRequest :: BS8.ByteString -> BTTrackerRequest -> IO String
makeRequest url req = do
    response <- simpleHTTP request
    getResponseBody response
    where
        urlString = BS8.unpack url
        request = getRequest $ urlString ++ makeQueryString req