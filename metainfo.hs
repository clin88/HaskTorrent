{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
module Metainfo (loadMetainfoFile, loadMetainfo, BTMetainfo(), BTInfo(), BTFileinfo()) where

import           Control.Applicative ((<$>), (<*>))
import           Data.BEncode        as BE
import           Data.BEncode.BDict  as BD (BDictMap, lookup)
import           Data.ByteString     as BS (ByteString, readFile)
import           Data.Typeable       (Typeable)

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
    { files       :: [BTFileinfo]
    , name        :: ByteString
    , pieceLength :: Int
    , pieces      :: ByteString
    , private     :: Maybe Bool } deriving (Typeable, Show)

{- toBEncode is probably not working for either of these classes. But they must
be defined for library to work. -}
instance BEncode BTInfo where
    toBEncode BTInfo {..} = toDict $
        "files" .=! files .:
        "name" .=! name .:
        "piece length" .=! pieceLength .:
        "pieces" .=! pieces .:
        "private" .=? private .:
        endDict

    -- I can't get this to work either, but fuck it.
    fromBEncode = fromDict $ BTInfo
        <$>! "files"
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
loadMetainfoFile fn = decode <$> BS.readFile fn

loadMetainfo :: ByteString -> Result BTMetainfo
loadMetainfo = decode

-- testing helper functions, coz I don't know how to write tests yet.
getBTDict :: ByteString -> BDictMap BValue
getBTDict inp = either error id $ do
    (BDict dct) <- decode inp
    return dct

getFileinfoDict = do
    content <- BS.readFile "test.torrent"
    dct <- return $ getBTDict content
    (Just (BDict infoDct)) <- return $ BD.lookup "info" dct
    (Just (BList fileList)) <- return $ BD.lookup "files" infoDct
    return $ fileList !! 0

getInfoDict = do
    content <- BS.readFile "test.torrent"
    dct <- return $ getBTDict content
    (Just x) <- return $ BD.lookup "info" dct
    return x

getDict = do
    content <- BS.readFile "test.torrent"
    dct <- return $ (decode content :: Result BValue)
    return $ either error id dct
