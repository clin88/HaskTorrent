{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Testing where
import Control.Exception (try, IOException, handle)
import           Control.Monad         (liftM)
import           Crypto.Hash.SHA1      (hashlazy)
import           Data.ByteString.Char8 (ByteString, unpack)
import           Metainfo              (BTMetainfo (..),
                                        loadMetainfoFile, totalSize, trackers)
import Network.HTTP (getResponseBody)
import           Tracker               (BTTrackerRequest (..),
                                        makeRequest,
                                        getTrackerRequest,
                                        BTTrackerResponse (..))
import Data.BEncode

testF = do
    Right minfo <- loadMetainfoFile "test.torrent"
    request <- return $ getTrackerRequest minfo
    return (request, minfo)

testRequest url = do
    Right minfo <- loadMetainfoFile "test.torrent"
    request <- return $ getTrackerRequest minfo
    response <- makeRequest url request
    return response

tryAllUrls = do
    (req, minfo) <- testF
    let safeRequest = try . testRequest :: ByteString -> IO (Either IOException String)
    let handleRequest = handle (\e -> do { print (e :: IOException) ; return "" }) . testRequest
    responses <- mapM handleRequest $ trackers minfo
    print responses
    return responses

testdata :: ByteString
testdata = "d8:completei11e10:incompletei6e8:intervali1800e12:min intervali1800e5:peers102:Ey\\\135\200\213@\131\206\149\242eJ\212\183\186\NUL\NULI1\220\130\224}G\173\\\182F\151\173DK\230\161\254;eK\253\203FR/\161\131\&8\158\STX`\182K\218\201\190\167\168\RS.\SO)D\158\238\202\234\182\239\201\ACK\SUB\225OR\FS\a\216\FS)D\STX\t\162\188U\ETB\148\242\155Di\226c[\228\152\151\228\&0\244M\222e\n"