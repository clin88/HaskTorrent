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
    Right minfo <- loadMetainfoFile "test.torrent"
    return minfo

testTrackerRequest :: IO BTTrackerRequest
testTrackerRequest = do
    Right minfo <- loadMetainfoFile "test.torrent"
    return $ makeRequestObject minfo

testRequest url = do
    Right minfo <- loadMetainfoFile "test.torrent"
    request <- return $ makeRequestObject minfo
    response <- makeRequest request url
    return response

testGetMultipleResponses :: IO [BTTrackerResponse]
testGetMultipleResponses = do
    minfo <- testMetainfo
    announceAllTrackers minfo

testGetAllPeers :: IO [Peer]
testGetAllPeers = extractAllPeers <$> testGetMultipleResponses

testTrackerResponse :: BS.ByteString
testTrackerResponse = "d8:completei11e10:incompletei6e8:intervali1800e12:min intervali1800e5:peers102:Ey\\\135\200\213@\131\206\149\242eJ\212\183\186\NUL\NULI1\220\130\224}G\173\\\182F\151\173DK\230\161\254;eK\253\203FR/\161\131\&8\158\STX`\182K\218\201\190\167\168\RS.\SO)D\158\238\202\234\182\239\201\ACK\SUB\225OR\FS\a\216\FS)D\STX\t\162\188U\ETB\148\242\155Di\226c[\228\152\151\228\&0\244M\222e\n"

testPeerListRaw :: BS.ByteString
testPeerListRaw = "Ey\\\135\200\213@\131\206\149\242eJ\212\183\186\NUL\NULI1\220\130\224}G\173\\\182F\151\173DK\230\161\254;eK\253\203FR/\161\131\&8\158\STX`\182K\218\201\190\167\168\RS.\SO)D\158\238\202\234\182\239\201\ACK\SUB\225OR\FS\a\216\FS)D\STX\t\162\188U\ETB\148\242\155Di\226c[\228\152\151\228\&0\244M\222e\n"

myPeerId = "HT123456789012345678"

formHandshake = do
    minfo <- testMetainfo
    return $ Handshake
        { protocolId = "BitTorrent protocol"
        , protocolParams = 0
        , infoHash = (BSL.fromStrict $ MI.infoHash minfo)
        , peerId = myPeerId }

testHandshake = do
    resps <- testGetMultipleResponses
    let testpeer = extractAllPeers resps !! 0
    print testpeer
    handle <- connectToPeer testpeer
    myhandshake <- formHandshake
    handshakeToPeer handle myhandshake

{-
getPeers :: BValue -> Peers
getPeers (BString bs) = Peers $ getPeers' $ BS.unpack bs

getPeers' :: [Word8] -> [Peer]
getPeers' [] = []
getPeers' (splitAt -> (peer, peers)) = procPeer peer:getPeers' peers
    where
        procPeer (splitAt -> (ip, port)) = Peer { peerIp = ip, peerPort = unpackPort port }
        unpackPort (a, b) = a * 256 + b
-}
