{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
module Metainfo
    --( loadMetainfoFile
    --, loadMetainfo
    --, BTMetainfo
    --, totalSize
    --, infoHash
    --, trackers
    --) where
where

import Control.Applicative   ((<$>), (<*>))
import Crypto.Hash.SHA1      (hashlazy)
import Data.BEncode          as BE
import Data.ByteString.Char8 as BS8 (readFile)
import Data.ByteString       (ByteString)
import Data.ByteString       as B (splitAt)
import Data.Maybe            (fromMaybe)
import Data.Typeable         (Typeable)


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

data BTInfo = BTInfo
    { btiFiles       :: [BTFileinfo]
    , btiName        :: ByteString
    , btiPiecelength :: Int
    , btiPieces      :: [ByteString]
    , btiPrivate     :: Maybe Bool } deriving (Typeable, Show)

instance BEncode BTInfo where
    toBEncode BTInfo {..} = toDict $
        "files"        .=! btiFiles       .:
        "name"         .=! btiName        .:
        "piece length" .=! btiPiecelength .:
        "pieces"       .=! btiPieces      .:
        "private"      .=? btiPrivate     .:
        endDict

    fromBEncode = fromDict $ do
        files <- lookAhead $ opt "files"
        case files of
            Just _ -> BTInfo
                        <$>! "files"
                        <*>! "name"
                        <*>! "piece length"
                        <*>! "pieces"
                        <*>? "private"
            Nothing -> do
                finfo <- BTFileinfo <$>! "length"
                                    <*>? "md5sum"
                                    <*> do x <- field $ req "name"
                                           return [x]
                BTInfo [finfo] "." <$>! "piece length"
                                   <*> do
                                       pieces <- field $ req "pieces"
                                       return $ chunk 20 pieces
                                   <*>? "private"

chunk :: Int -> ByteString -> [ByteString]
chunk n bs = chunk' [] $ B.splitAt n bs
    where
        chunk' acc (piece, "")     = acc
        chunk' acc (piece, remain) = chunk' (piece:acc) $ B.splitAt n remain

data BTFileinfo = BTFileinfo
    { filelen :: Int
    , fMD5sum :: Maybe ByteString
    , path    :: [ByteString] } deriving (Typeable, Show)

instance BEncode BTFileinfo where
    toBEncode BTFileinfo {..} = toDict $
        "length" .=! filelen .:
        "md5sum" .=? fMD5sum .:
        "path"   .=! path    .:
        endDict

    fromBEncode = fromDict $ BTFileinfo
        <$>! "length"
        <*>? "md5sum"
        <*>! "path"

{- MAIN EXPORTS -}

loadMetainfoFile :: String -> IO BTMetainfo
loadMetainfoFile fn = do
    file <- BS8.readFile fn
    return $ either error id $ decode file

{- HELPER FUNCTIONS -}

infoHash :: BTMetainfo -> ByteString
infoHash = hashlazy . encode . info

totalSize :: BTMetainfo -> Int
totalSize BTMetainfo {info = BTInfo {..}} = totalSize' 0 btiFiles
    where
        totalSize' acc [] = acc
        totalSize' acc (f:fs) = totalSize' (acc + filelen f) fs

trackers :: BTMetainfo -> [ByteString]
trackers BTMetainfo {..} = announce:concat list
    where list = fromMaybe [] announceList
