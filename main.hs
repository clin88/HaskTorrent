{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Async (race_)
import           Control.Concurrent.STM
import           Control.Exception        (bracket, finally)
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

logger :: PeerAddr -> String -> IO ()
logger pa msg = printf "(%s) %s \n" (peerHost $ pa) msg

launchPeer :: ByteString -> PeerAddr -> TVar PiecesMap -> IO (ThreadId, TVar Peer)
launchPeer infohash pa@(PeerAddr {..}) tPieces = do
    pieces <- readTVarIO tPieces
    tPeer <- newTVarIO $ defaultPeer pieces -- snapshot of global state for which to diff with
    tid <- forkFinally (go tPeer) $ \ee -> case ee of
        Left e   -> logger pa (printf "thread crashed: %s" $ show e)
        Right () -> logger pa "thread finished"
    return (tid, tPeer)
    where
        -- bracket to protect socket
        go :: TVar Peer -> IO ()
        go tPeer = bracket (      do logger pa "connectTo"; connectTo peerHost peerPort)
                           (\h -> do logger pa "hClose"   ; hClose h)
                           (gogo tPeer)
        -- initialize friendship; finally mop up
        gogo :: TVar Peer -> Handle -> IO ()
        gogo tPeer h = do
            hSetBinaryMode h True
            logger pa "peerHandshake"
            peerHandshake h infohash peerid peerTrackerId
            -- TODO: Better exception handling here.
            -- TODO: also send our bitfield
            finally (gogogo tPeer h)
                    (mopup pa tPeer)
        -- launch the two sub threads
        gogogo :: TVar Peer -> Handle -> IO ()
        gogogo tPeer h = do
            logger pa "DaiJouBu"
            race_ (peerController tPeer tPieces h)
                  (peerListener tPeer h)

-- relinquish stale claims
mopup :: PeerAddr -> TVar Peer -> IO ()
mopup pa tPeer = do
    logger pa "TODO: relinquish claims"
    return ()

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


