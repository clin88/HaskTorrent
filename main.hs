{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
import           Control.Monad      (liftM)
import           Crypto.Hash.SHA1   (hashlazy)
import qualified Data.ByteString    as BS (ByteString, unpack)
import           Metainfo           (BTMetainfo (..), loadMetainfoFile,
                                     totalSize)
import           System.Environment (getArgs)
import           Tracker            (BTTrackerRequest (..), makeRequestObject,
                                     makeRequest)

getBTFileName :: IO String
getBTFileName = do
    as <- getArgs
    case as of
        [] -> do print "Specify a bittorrent file please!"; fail "No BT file."
        (x:_) -> return x

--main = do
--    fname <- getBTFileName
--    eMetainfo <- loadMetainfoFile fname
--    Right (url, request) <- return $ liftM getTrackerRequest eMetainfo
--    return ()


