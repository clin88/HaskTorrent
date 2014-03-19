{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Async (race_)
import           Control.Concurrent.STM
import           Control.Exception        (bracket)
import           Control.Monad
import           Crypto.Hash.SHA1         (hashlazy)
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS (ByteString, take, unpack)
import qualified Data.IntMap              as IM
import           Data.IntMap.Strict       (IntMap)
import           Metainfo
import           Network                  (connectTo)
import           Peers
import           System.Environment       (getArgs)
import           System.IO
import           Text.Printf              (printf)
import           Tracker

peerid :: ByteString
peerid = BS.take 20 "Many were increasingly of the opinion that they'd all made a big mistake"

launchPeer :: ByteString -> PeerAddr -> TVar PiecesMap -> IO (ThreadId, TVar Peer)
launchPeer infohash pa@(PeerAddr {..}) tGlPieces = do
    glPieces <- readTVarIO tGlPieces
    peer <- newTVarIO $ defaultPeer glPieces
    -- TODO: bracket doesn't call close if open crashes
    let open = do
            printf "OPENING: %s\n" (show pa)
            handle <- connectTo peerHost peerPort
            hSetBinaryMode handle True
            -- TODO: Better exception handling here.
            printf "HANDSHAKING: %s\n" (show pa)
            peerHandshake handle infohash peerid peerTrackerId
            printf "DAIJOUBU: %s\n" (show pa)
            return handle
        action handle = do
            -- TODO: also send our bitfield
            race_ (peerController peer tGlPieces handle)
                  (peerListener peer handle)
        close h = do
            -- TODO: Ensure claims are relinquished.
            printf "CLOSE: %s\n" (show pa)
            hClose h
    -- nested bracket: inner handshake -> claim; outer handles socket
    tid <- forkFinally (bracket open close action) $ \_ -> do
        printf "FINISHED: %s\n" (show pa)
    return (tid, peer)

getBTFileName :: IO String
getBTFileName = do
    as <- getArgs
    case as of
        [] -> do print "Specify a bittorrent file please!"; fail "No BT file."
        (x:_) -> return x

main = do
    fname <- getBTFileName
    Right minfo <- loadMetainfoFile fname
    peers <- extractAllPeers <$> announceAllTrackers minfo
    printf "PEERS: %s..\n" (take 50 $ show peers)
    let (BTSingleFileInfo {..}) = info minfo
        infohash = infoHash minfo
        pieces = fmap (\_ -> Unclaimed)
               . IM.fromList
               . zip [0..]
               $ [0,sfPieceLength..sfFileLength] -- one extra
    printf "PIECES: %s..\n" (take 50 $ show pieces)
    tGlPieces <- newTVarIO pieces
    forM_ peers $ \p -> do
        launchPeer infohash p tGlPieces
    forever $ do
        threadDelay $ round 5e6
    -- Right (url, request) <- return $ liftM getTrackerRequest eMetainfo
    -- return ()


