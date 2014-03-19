{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
import Text.Printf (printf)
import Data.IntMap.Strict (IntMap)
import           Control.Monad      (liftM)
import           Crypto.Hash.SHA1   (hashlazy)
import qualified Data.ByteString    as BS (ByteString, unpack, take)
import Data.ByteString (ByteString)
import           Metainfo           (BTMetainfo (..), loadMetainfoFile,
                                     totalSize)
import           System.Environment (getArgs)
import           Tracker            (BTTrackerRequest (..), makeRequestObject,
                                     makeRequest)
import Peers
import Control.Exception (bracket)
import Control.Concurrent
import Control.Concurrent.Async (race_)
import Control.Concurrent.STM
import Network (connectTo)
import System.IO
import Tracker
import Metainfo
import Control.Applicative
import Control.Monad
import qualified Data.IntMap as IM

peerid :: ByteString
peerid = BS.take 20 "Many were increasingly of the opinion that they'd all made a big mistake"

launchPeer :: ByteString -> PeerAddr -> TVar PiecesMap -> IO (ThreadId, TVar Peer)
launchPeer infohash pa@(PeerAddr {..}) tGlPieces = do
    glPieces <- readTVarIO tGlPieces
    peer <- newTVarIO $ defaultPeer glPieces
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
            printf "CLOSE: %s\n" (show pa)
            hClose h
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
        printf "so ronery\n"
    -- Right (url, request) <- return $ liftM getTrackerRequest eMetainfo
    -- return ()


