{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
module Metainfo where

import           Data.BEncode       as BE
import           Data.BEncode.BDict as BD (BDictMap, lookup)
import           Data.ByteString    as BS (ByteString, readFile)
import           Data.Typeable      (Typeable)



data BTInfo = BTInfo
    { files       :: [Fileinfo]
    , name        :: ByteString
    , pieces      :: ByteString
    , pieceLength :: Int
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

data Fileinfo = Fileinfo
    { filelen :: Int
    , fMD5sum :: Maybe ByteString
    , path    :: [ByteString] } deriving (Typeable, Show)

instance BEncode Fileinfo where
    toBEncode Fileinfo {..} = toDict $
        "length" .=! filelen .:
        "md5sum" .=? fMD5sum .:
        "path"   .=! path .:
        endDict

    fromBEncode = fromDict $ Fileinfo
        <$>! "length"
        <*>? "md5sum"
        <*>! "path"

getBTDict :: ByteString -> BDictMap BValue
getBTDict inp = either error id $ do
    (BDict dct) <- decode inp
    return dct

--getFileinfoDict :: BDictMap BValue
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

loadMetainfo :: ByteString -> Result BTInfo
loadMetainfo = BTInfo

    do
    content <- BS.readFile "test.torrent"
    return $ decode content
