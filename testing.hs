{-# LANGUAGE RecordWildCards #-}
import           Control.Monad         (liftM)
import           Crypto.Hash.SHA1      (hashlazy)
import           Data.ByteString.Char8 (ByteString, unpack)
import           Metainfo              (BTMetainfo (..),
                                        loadMetainfoFile, totalSize, trackers)
import           System.Environment    (getArgs)
import           Tracker               (BTEvents (..), BTTrackerRequest (..),
                                        NSBool (..), makeRequest,
                                        makeQueryString, getTrackerRequest)

testF = do
    Right minfo <- loadMetainfoFile "test.torrent"
    request <- return $ getTrackerRequest minfo
    url <- return $ announce minfo
    return (url, request, minfo)

testRequest url = do
    Right minfo <- loadMetainfoFile "test.torrent"
    request <- return $ getTrackerRequest minfo
    print $ makeQueryString request
    makeRequest url request
