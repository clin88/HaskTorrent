{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
import           BTUtils
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

getBTFileName :: IO String
getBTFileName = do
    as <- getArgs
    case as of
        [] -> do print "Specify a bittorrent file please!"; fail "No BT file."
        (x:_) -> return x

launchPeer :: ByteString -> TVar PiecesMap -> PeerAddr -> IO (ThreadId, TVar Peer)
launchPeer infohash tPieces pa@(PeerAddr {..}) = do
    pieces <- readTVarIO tPieces
    tPeer <- newTVarIO $ defaultPeer pieces -- snapshot of global state for which to diff with
    tid <- forkFinally (connect tPeer) $ \ee -> case ee of
        Left e   -> logger peerHost (printf "thread crashed: %s" $ show e)
        Right () -> logger peerHost "thread finished"
    return (tid, tPeer)
    where
        -- bracket to protect socket
        connect :: TVar Peer -> IO ()
        connect tPeer = bracket (      do logger peerHost "connectTo"; connectTo peerHost peerPort)
                                (\h -> do logger peerHost "hClose"   ; hClose h)
                                (initRelationship tPeer)
        -- initialize friendship; finally mop up
        initRelationship :: TVar Peer -> Handle -> IO ()
        initRelationship tPeer h = do
            hSetBinaryMode h True
            logger peerHost "peerHandshake"
            peerHandshake h infohash peerid peerTrackerId
            -- TODO: Better exception handling here.
            -- TODO: also send our bitfield
            finally (startPeerManager tPeer h)
                    (mopup tPeer)
        -- launch the two sub threads
        startPeerManager :: TVar Peer -> Handle -> IO ()
        startPeerManager tPeer h = do
            logger peerHost "DaiJouBu"
            race_ (peerController tPeer tPieces h)
                  (peerListener tPeer h)
        -- relinquish claims
        mopup :: TVar Peer -> IO ()
        mopup tPeer = do
            logger peerHost "TODO: relinquish claims"
            return ()

main = do
    fname <- getBTFileName
    Right minfo <- loadMetainfoFile fname
    peers <- extractAllPeers <$> announceAllTrackers minfo
    printf "PEERS: %s..\n" (take 50 $ show peers)
    let (BTSingleFileInfo {..}) = info minfo
        infohash = infoHash minfo
        pieces = fmap (const Unclaimed)
               . IM.fromList
               . zip [0..]
               $ [0,sfPieceLength..sfFileLength] -- one extra
    printf "PIECES: %s..\n" (take 50 $ show pieces)
    tGlPieces <- newTVarIO pieces
    forM_ peers $ launchPeer infohash tGlPieces
    forever $ do
        threadDelay $ round 5e6
        glPieces <- readTVarIO tGlPieces
        print glPieces
    -- Right (url, request) <- return $ liftM getTrackerRequest eMetainfo
    -- return ()


