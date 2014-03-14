{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
import           Control.Applicative
import           Control.Concurrent.Async (mapConcurrently)
import           Control.Exception        (IOException, throwIO, try)
import           Control.Monad            (liftM)
import           Crypto.Hash.SHA1         (hashlazy)
import           Data.BEncode
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as BSL
import           Data.Either              (rights)
import           Data.List.Split
import           Data.Word                (Word16, Word8)
import           Metainfo                 hiding (infoHash)
import qualified Metainfo                 as MI (infoHash)
import           Peers
import           Tracker

testMetainfo :: IO BTMetainfo
testMetainfo = do
    result <- loadMetainfoFile "test2.torrent"
    case result of
        Right minfo -> return minfo
        Left e -> do fail e

testTrackerRequest :: IO BTTrackerRequest
testTrackerRequest = do
    Right minfo <- loadMetainfoFile "test2.torrent"
    return $ makeRequestObject minfo

testRequest url = do
    Right minfo <- loadMetainfoFile "test2.torrent"
    request <- return $ makeRequestObject minfo
    response <- makeRequest request url
    return response

testGetMultipleResponses :: IO [BTTrackerResponse]
testGetMultipleResponses = do
    minfo <- testMetainfo
    announceAllTrackers minfo

testGetAllPeers :: IO [PeerAddr]
testGetAllPeers = extractAllPeers <$> testGetMultipleResponses

testTrackerResponse :: BS.ByteString
testTrackerResponse = "d8:completei11e10:incompletei6e8:intervali1800e12:min intervali1800e5:peers102:Ey\\\135\200\213@\131\206\149\242eJ\212\183\186\NUL\NULI1\220\130\224}G\173\\\182F\151\173DK\230\161\254;eK\253\203FR/\161\131\&8\158\STX`\182K\218\201\190\167\168\RS.\SO)D\158\238\202\234\182\239\201\ACK\SUB\225OR\FS\a\216\FS)D\STX\t\162\188U\ETB\148\242\155Di\226c[\228\152\151\228\&0\244M\222e\n"

testPeerListRaw :: BS.ByteString
testPeerListRaw = "Ey\\\135\200\213@\131\206\149\242eJ\212\183\186\NUL\NULI1\220\130\224}G\173\\\182F\151\173DK\230\161\254;eK\253\203FR/\161\131\&8\158\STX`\182K\218\201\190\167\168\RS.\SO)D\158\238\202\234\182\239\201\ACK\SUB\225OR\FS\a\216\FS)D\STX\t\162\188U\ETB\148\242\155Di\226c[\228\152\151\228\&0\244M\222e\n"

--testConnectToPeers :: BTMetainfo -> [PeerAddr] -> IO [Peer]
testConnectToPeers minfo = connectToPeers hs infoh
    where
        infoh = MI.infoHash minfo
        hs = formHandshake infoh myPeerId

testUgh = do
    minfo <- testMetainfo
    peers <- testGetAllPeers
    testConnectToPeers minfo peers

myPeerId = "HT123456789012345678"
