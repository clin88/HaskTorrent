{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Tracker
    ( makeRequest
    , BTTrackerRequest (..)
    , BTEvents (..)
    , NSBool (..)
    ) where

import           Data.ByteString   (ByteString)
import           Data.String       (IsString)
import           Network.HTTP      (getRequest, simpleHTTP)
import           Network.HTTP.Base (urlEncodeVars)

data BTTrackerRequest = BTTrackerRequest
    { info_hash  :: ByteString
    , peer_id    :: ByteString
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

requestQueryString :: BTTrackerRequest -> String
requestQueryString BTTrackerRequest {..} = urlEncodeVars $
    [ ("info_hash", show info_hash)
    , ("peer_id", show peer_id)
    , ("port", show port)
    , ("uploaded", show uploaded)
    , ("downloaded", show downloaded)
    , ("left", show left)
    , ("compact", show compact)
    , ("no_peer_id", show no_peer_id)
    , ("event", show event) ]

--makeRequest :: String -> BTTrackerRequest -> IO ( (b c))
makeRequest url req = simpleHTTP request
    where
        request = getRequest $ url ++ "?" ++ requestQueryString req
