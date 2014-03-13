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
import qualified Data.ByteString.Char8 as BS8 (readFile)
import           Data.Maybe            (fromMaybe)
import           Data.Typeable         (Typeable)
import           Network.HTTP.Types    (urlEncode)
import Data.ByteString (ByteString)

data BTMetainfo = BTMetainfo
    { announce     :: ByteString
    , announceList :: Maybe [[ByteString]]
    , comment      :: Maybe ByteString
    , createdBy    :: Maybe ByteString
    , creationDate :: Maybe ByteString
    , encoding     :: Maybe ByteString
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

data BTInfo =
    BTSingleFileInfo
    { sfFileLength :: Int
    , sfMd5sum :: Maybe ByteString
    , sfName :: ByteString
    , sfPieceLength :: Int
    , sfPieces :: ByteString
    , sfPrivate :: Maybe Bool } |
    BTMultiFileInfo
    { mfFiles       :: [BTFileinfo]
    , mfName        :: ByteString
    , mfPieceLength :: Int
    , mfPieces      :: ByteString
    , mfPrivate     :: Maybe Bool } deriving (Typeable, Show)

instance BEncode BTInfo where
    toBEncode BTSingleFileInfo {..} = toDict $
        "length" .=! sfFileLength .:
        "md5sum" .=? sfMd5sum .:
        "name" .=!  sfName .:
        "piece length" .=! sfPieceLength .:
        "pieces" .=! sfPieces .:
        "private" .=? sfPrivate .:
        endDict

    toBEncode BTMultiFileInfo {..} = toDict $
        "files" .=! mfFiles .:
        "name" .=! mfName .:
        "piece length" .=! mfPieceLength .:
        "pieces" .=! mfPieces .:
        "private" .=? mfPrivate .:
        endDict

    fromBEncode = fromDict $ do
        files <- opt "files"
        case files of
            Just _ -> BTMultiFileInfo
                        <$>! "files"
                        <*>! "name"
                        <*>! "piece length"
                        <*>! "pieces"
                        <*>? "private"
            Nothing -> BTSingleFileInfo
                        <$>! "length"
                        <*>? "md5sum"
                        <*>! "name"
                        <*>! "piece length"
                        <*>! "pieces"
                        <*>? "private"

data BTFileinfo = BTFileinfo
    { filelen :: Int
    , fMD5sum :: Maybe ByteString
    , path    :: [ByteString] } deriving (Typeable, Show)

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

loadMetainfo :: ByteString -> Result BTMetainfo
loadMetainfo = decode

{- HELPER FUNCTIONS -}
infoHash :: BTMetainfo -> ByteString
infoHash = hashlazy . encode . info

totalSize :: BTMetainfo -> Int
totalSize minfo = case info minfo of
    BTSingleFileInfo {..} -> sfFileLength
    BTMultiFileInfo {..} -> totalSize' 0 mfFiles
    where
        totalSize' acc [] = acc
        totalSize' acc (f:fs) = totalSize' (acc + filelen f) fs

trackers :: BTMetainfo -> [ByteString]
trackers BTMetainfo {..} = announce:concat list
    where list = fromMaybe [] announceList
