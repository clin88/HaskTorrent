{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
module Metainfo where
--    ( loadMetainfoFile
--    , loadMetainfo
--    , BTMetainfo(..)
--    , totalSize
--    , infoHash
--    , trackers
--    ) where

import           Control.Applicative   ((<$>), (<*>))
import           Crypto.Hash.SHA1      (hashlazy)
import           Data.BEncode          as BE
import           Data.BEncode.BDict    as BD (BDictMap, lookup)
import qualified Data.ByteString.Char8 as BS8 (ByteString, readFile)
import           Data.Maybe            (fromMaybe)
import           Data.Typeable         (Typeable)
import           Network.HTTP.Types    (urlEncode)

data BTMetainfo = BTMetainfo
    { announce     :: BS8.ByteString
    , announceList :: Maybe [[BS8.ByteString]]
    , comment      :: Maybe BS8.ByteString
    , createdBy    :: Maybe BS8.ByteString
    , creationDate :: Maybe BS8.ByteString
    , encoding     :: Maybe BS8.ByteString
    , info         :: BTInfo } deriving (Typeable, Show)

instance BEncode BTMetainfo where
    toBEncode BTMetainfo {..} = toDict $
        "announce" .=! announce .:
        "announce-list" .=? announceList .:
        "comment" .=? comment .:
        "created by" .=? createdBy .:
        "creation date" .=? creationDate .:
        "encoding" .=? encoding .:
        "info" .=! info .:
        endDict

    fromBEncode = fromDict $ BTMetainfo
        <$>! "announce"
        <*>? "announce-list"
        <*>? "comment"
        <*>? "created by"
        <*>? "creation date"
        <*>? "encoding"
        <*>! "info"

data BTInfo = BTInfo
    { files       :: [BTFileinfo]
    , name        :: BS8.ByteString
    , pieceLength :: Int
    , pieces      :: BS8.ByteString
    , private     :: Maybe Bool } deriving (Typeable, Show)

instance BEncode BTInfo where
    toBEncode BTInfo {..} = toDict $
        "files" .=! files .:
        "name" .=! name .:
        "piece length" .=! pieceLength .:
        "pieces" .=! pieces .:
        "private" .=? private .:
        endDict

    fromBEncode = fromDict $ BTInfo
        <$>! "files"
        <*>! "name"
        <*>! "piece length"
        <*>! "pieces"
        <*>? "private"

data BTFileinfo = BTFileinfo
    { filelen :: Int
    , fMD5sum :: Maybe BS8.ByteString
    , path    :: [BS8.ByteString] } deriving (Typeable, Show)

instance BEncode BTFileinfo where
    toBEncode BTFileinfo {..} = toDict $
        "length" .=! filelen .:
        "md5sum" .=? fMD5sum .:
        "path"   .=! path .:
        endDict

    fromBEncode = fromDict $ BTFileinfo
        <$>! "length"
        <*>? "md5sum"
        <*>! "path"

loadMetainfoFile :: String -> IO (Result BTMetainfo)
loadMetainfoFile fn = decode <$> BS8.readFile fn

loadMetainfo :: BS8.ByteString -> Result BTMetainfo
loadMetainfo = decode

{- HELPER FUNCTIONS -}

infoHash :: BTInfo -> BS8.ByteString
infoHash btinfo = hashlazy $ encode btinfo

totalSize :: BTMetainfo -> Int
totalSize minfo = totalSize' 0 $ files . info $ minfo
    where
        totalSize' acc [] = acc
        totalSize' acc (f:fs) = totalSize' (acc + filelen f) fs

trackers :: BTMetainfo -> [BS8.ByteString]
trackers BTMetainfo {..} = announce:concat list
    where list = fromMaybe [] announceList
