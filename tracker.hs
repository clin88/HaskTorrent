{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Tracker
    ( makeRequest
    , BTTrackerRequest (..)
    , BTEvents (..)
    , NSBool (..)
    , makeQueryString
    , getTrackerRequest
    ) where

import           Data.BEncode          (BEncode, decode)
import qualified Data.ByteString.Char8 as BS8 (ByteString, pack, unpack)
import           Metainfo              (BTMetainfo (..), infoHash, totalSize,
                                        trackers)
import           Network.HTTP          (getRequest, getResponseBody, simpleHTTP)
import           Network.HTTP.Types    (renderSimpleQuery)

data BTTrackerResponse =
    BTTrackerResponseFailure
    { failureReason :: BS8.ByteString } |
    BTTrackerResponse
    { warningMessage :: BS8.ByteString
    , interval       :: Int
    , minInterval    :: Int
    , trackerId      :: Int
    , complete       :: Int
    , incomplete     :: Int
    , peers          :: [Peer]
    }
    deriving (Show)

--instance BEncode Data where
--    func =

newtype Peer = Peer { unPeer :: BS8.ByteString } deriving (Eq, Show)

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

--makeRequest :: BS8.ByteString -> BTTrackerRequest -> Either String BTTrackerResponse
makeRequest url req = do
    response <- simpleHTTP request
    getResponseBody response
    where
        urlString = BS8.unpack url
        request = getRequest $ urlString ++ makeQueryString req
