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
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq

peerid :: ByteString
peerid = BS.take 20 "Many were increasingly of the opinion that they'd all made a big mistake"

getBTFileName :: IO String
getBTFileName = do
    as <- getArgs
    case as of
        [] -> do print "Specify a bittorrent file please!"; fail "No BT file."
        (x:_) -> return x

launchPeer :: ByteString -> TVar GlobalPieces -> PeerAddr -> IO (ThreadId, TVar Peer)
launchPeer infohash tPieces pa@(PeerAddr {..}) = do
    pieces <- readTVarIO tPieces
    tPeer <- newTVarIO $ defaultPeer pieces -- snapshot of global state for which to diff with
    tid <- forkFinally (connect tPeer) $ \e -> case e of
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

buildFilePieces :: Int -> BTFileinfo -> Seq PieceInfo
buildFilePieces plen (BTFileinfo {..}) =
    case extra of
        0 -> front
        _ -> front |> PieceInfo extra Unclaimed
    where
        front = Seq.replicate wholepieces piece
        (wholepieces, extra) = filelen `divMod` plen
        piece = PieceInfo plen Unclaimed

main = do
    fname <- getBTFileName
    Right minfo@BTMetainfo {..} <- loadMetainfoFile fname

    let infohash = infoHash minfo
        pieces   = case info of
            BTSingleFileInfo {..} -> msum $ buildFilePieces sfPieceLength <$> [BTFileinfo sfFileLength sfMd5sum [sfName]]
            BTMultiFileInfo  {..} -> msum $ buildFilePieces mfPieceLength <$> mfFiles

    peers <- extractAllPeers <$> announceAllTrackers minfo
    tPieces <- newTVarIO pieces
    forM_ peers $ launchPeer infohash tPieces

    printf "PEERS: %s\n" (show peers)

    forever $ do
        threadDelay $ round 5e6
        --pieces <- readTVarIO tPieces
        --printf "PIECES: %s..\n" (take 50 $ show pieces)
    return ()
