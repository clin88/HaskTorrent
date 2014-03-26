{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns #-}
module Peers
    --( PeerAddr (..)
    --, Peer
    --, connectToPeer)
where

import Prelude hiding (or, and)

import Text.Printf            (printf)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad          (forM_, forever, join)
import Data.ByteString        (ByteString)
import Data.Maybe             (fromMaybe, isJust)
import Data.Monoid            ((<>))
import Data.Sequence          (Seq, (|>), ViewL((:<)))
import Data.Word
import Data.Foldable          (or, and, foldlM)

import qualified Data.Binary            as BIN
import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as L
import qualified Data.Sequence          as Seq

import           Network                (HostName, PortID (..))
import           System.IO              (Handle)

import           PeerMsgs
-- import           BTUtils

data PeerAddr = PeerAddr { peerHost      :: HostName
                         , peerPort      :: PortID
                         , peerTrackerId :: Maybe ByteString
                         } deriving (Show)

data PieceStatus = Claimed
                 | Downloaded
                 | Unclaimed
                 deriving (Eq, Show)

data PieceInfo = PieceInfo { pinfoLength :: Int
                           , pinfoStatus :: PieceStatus
                           } deriving (Eq, Show)

type PeerPieces   = Seq Bool
type GlobalPieces = Seq PieceInfo
type PieceID      = Int

data BlockStatus = BlockUnrequested
                 | BlockRequested
                 | BlockDownloaded ByteString
                 deriving (Eq, Show)

data BlockInfo = BlockInfo { binfoBlock  :: Block
                           , binfoStatus :: BlockStatus
                           } deriving (Eq, Show)

data Peer = Peer
    { pAmChoking       :: Bool
    , pAmInterested    :: Bool
    , pChokingMe       :: Bool
    , pInterestedMe    :: Bool
    , pPeerPieces      :: PeerPieces
    , pGlobPiecesImage :: GlobalPieces
    , pReqsFrom        :: Seq Block
    , pReqsTo          :: Seq (TVar Bool, Block)
    , pBlocks          :: Maybe (Seq BlockInfo)
    }
    -- TODO: Need last send time, last receive time.


defaultPeer :: GlobalPieces -> Peer
defaultPeer pieces = Peer
    { pAmChoking       = True
    , pAmInterested    = False
    , pChokingMe       = True
    , pInterestedMe    = False
    , pPeerPieces      = Seq.empty
    , pGlobPiecesImage = pieces
    , pReqsFrom        = Seq.empty
    , pReqsTo          = Seq.empty
    , pBlocks = Nothing }

sendMsg :: Handle -> PeerMessage -> IO ()
sendMsg to msg = do
    printf "SENDING MSG: %s\n" (show msg)
    B.hPut to $ encodeMsg msg

---------------------------------------------------------
-- Potential issue: Time dependent actions like request expiration
-- may not be run if STM blocks through retries. Since STMs only wake up
-- when a TVar changes, not when times change, these actions may not happen
-- on time.
peerController :: TVar Peer -> TVar GlobalPieces -> Handle -> IO ()
peerController tPeer tPieces handle = forever $ do
    join . atomically $ do
        peer   <- readTVar tPeer
        pieces <- readTVar tPieces
        sendHaves peer tPeer pieces handle
            `orElse` manageInterest peer tPeer handle pieces 
            `orElse` reqCleanup     peer tPeer
            `orElse` claimPiece     peer tPeer pieces tPieces 
            `orElse` makeRequests   peer tPeer handle
            `orElse` sendResponse   peer tPeer handle
            `orElse` packPieces     peer tPeer

{- PEERCONTROLLER ACTIONS -}

sendHaves :: Peer -> TVar Peer -> GlobalPieces -> Handle -> STM (IO ())
sendHaves peer@(Peer {..}) tPeer pGlobPieces handle = do
    case findNewPieces pGlobPieces pGlobPiecesImage of
        []   -> retry
        pids -> do
            writeTVar tPeer $ peer { pGlobPiecesImage = pGlobPieces }
            return $ forM_ pids (sendMsg handle . Have)
  where
    findNewPieces :: GlobalPieces -> GlobalPieces -> [PieceID]
    findNewPieces new old =
        Seq.foldrWithIndex accum [] $ Seq.zipWith isNew new old

    accum pid True acc  = pid : acc
    accum _   _    acc = acc

    isNew :: PieceInfo -> PieceInfo -> Bool
    isNew (pinfoStatus -> Downloaded) (pinfoStatus -> Claimed)   =
        True
    isNew (pinfoStatus -> Downloaded) (pinfoStatus -> Unclaimed) =
        True
    isNew _ _ = False

---------------------------------------------------------

-- if peer has piece we want, gain interest.
-- if peer has no piece we want, lose interest.
manageInterest :: Peer -> TVar Peer -> Handle -> GlobalPieces -> STM (IO ())
manageInterest peer@(Peer {..}) tPeer handle pieces =
    case (pAmInterested, isInterested pieces pPeerPieces) of
         (False, True) -> gainInterest         -- Not interested now, am interested.
         (True, False) -> loseInterest         -- Interested now, lost interest.
         _             -> retry
  where
    gainInterest = do
            writeTVar tPeer (peer {pAmInterested = True})
            return $ sendMsg handle Interested

    loseInterest = do
            writeTVar tPeer (peer {pAmInterested = False})
            return $ sendMsg handle Uninterested

-- find what unclaimed pieces we could download from this peer
isInterested :: GlobalPieces -> PeerPieces -> Bool
isInterested pieces peerPieces =
    or $ Seq.zipWith interestedIn pieces peerPieces
  where
    interestedIn :: PieceInfo -> Bool -> Bool
    interestedIn (isUnclaimed -> True) True = True
    interestedIn _                     _    = False

---------------------------------------------------------

-- TODO: Clean up recieved requests as well.
reqCleanup :: Peer -> TVar Peer -> STM (IO ())
reqCleanup peer@(Peer {..}) tPeer = do
    (freshReqs, oldReqs) <- freshOld 
    case Seq.null oldReqs of
        True  -> retry                   -- no expired requests ==> we're all good
        False -> do
            writeTVar tPeer peer { pReqsTo = freshReqs
                                 , pBlocks = newBlocks oldReqs }
            return $ return ()
            -- return $ print $ "Cleaning up requests!" ++ (show $ fmap snd freshReqs)
  where
    freshOld :: STM (Seq (TVar Bool, Block), Seq Block)
    freshOld = foldlM part (Seq.empty, Seq.empty) pReqsTo
      where
        part (fresh, old) req@(tTimer, block) = do
            expired <- readTVar tTimer
            if expired then return (fresh, old |> block)
                       else return (fresh |> req, old)

    newBlocks :: Seq Block -> Maybe (Seq BlockInfo)
    newBlocks oldReqs = do
        blocks <- pBlocks
        let oldBlocks = Seq.findIndicesL isExpired blocks
        return $ foldr (switchBlockSt BlockUnrequested) blocks oldBlocks
      where
        isExpired (BlockInfo b _) = isJust $ Seq.elemIndexL b oldReqs

---------------------------------------------------------

-- Tries to claim a new piece if no claim currently
claimPiece :: Peer -> TVar Peer -> GlobalPieces -> TVar GlobalPieces -> STM(IO ())
claimPiece peer@(Peer {pBlocks = Nothing}) tPeer pieces tPieces =
    case Seq.findIndexL isUnclaimed pieces of
        Just pieceid -> updateState pieceid
        Nothing      -> retry
  where
    updateState :: PieceID -> STM(IO ())
    updateState pieceid = do
        let pinfo = Seq.index pieces pieceid
        writeTVar tPieces $ switchPieceSt Claimed pieceid pieces
        writeTVar tPeer $
            peer { pBlocks = Just $ newPiece pieceid pinfo }
        return $ print $ "CONTROLLER: Claiming piece " ++ show pieceid

claimPiece _ _ _ _ = retry

-- Initialize map for new pieces
newPiece :: PieceID -> PieceInfo -> Seq BlockInfo
newPiece pieceid (PieceInfo {..}) =
    case extra of
        0 -> front
        _ -> front |> endblockinfo
  where
    (wholeblocks, extra) = pinfoLength `divMod` 16384
    front = Seq.fromList $ map blockinfo [0..wholeblocks - 1]

    blockinfo n = BlockInfo
        { binfoBlock  = Block pieceid (16384*n) 16384
        , binfoStatus = BlockUnrequested }

    endblockinfo = BlockInfo
        { binfoBlock  = Block pieceid (16384*wholeblocks) extra
        , binfoStatus = BlockUnrequested }

---------------------------------------------------------

makeRequests :: Peer -> TVar Peer -> Handle -> STM(IO())
makeRequests peer@(Peer {..}) tPeer handle = do
    let room = roomInQueue pReqsTo
        nextBlock = unreqedBlock pBlocks
    case (room, pChokingMe, nextBlock, pAmInterested) of
        (True, False, Just (blockId, block), True) -> do
            tTimer <- newTVar False
            writeTVar tPeer peer { pReqsTo = pReqsTo |> (tTimer, block)
                                 , pBlocks = pBlocks >>= return . switchBlockSt BlockRequested blockId }
            return $ do
                -- Fork thread that rings timer after 20 seconds
                forkIO $ do
                    threadDelay $ 45 * 1000000 
                    atomically $ writeTVar tTimer True
                sendMsg handle $ Request block
        _                                          -> do
            -- unsafeIOToSTM $ do 
            --     printf "Failed to make request. room: %s choked: %s freeblock: %s interest: %s\n" (show room)
            --                                                                                       (show pChokingMe)
            --                                                                                       (show nextBlock)
            --                                                                                       (show pAmInterested)
            retry
  where
    roomInQueue q = Seq.length q <= 10

unreqedBlock :: Maybe (Seq BlockInfo) -> Maybe (Int, Block)
unreqedBlock curBlocks = do
    blocks <- curBlocks
    blockId <- Seq.findIndexL isUnrequested blocks
    let block = binfoBlock $ Seq.index blocks blockId
    return (blockId, block)

---------------------------------------------------------

-- writeTVar tPieces $ switchPieceSt Downloaded pieceID pieces
-- writeTChan tWriterChan $ packBlocks pBlocks
-- TODO: Check hash
-- TODO: Why isn't a complete piece getting packed?!
packPieces :: Peer -> TVar Peer -> STM(IO())
packPieces peer@Peer{..} tPeer 
    | allBlocksDLed pBlocks = do
        writeTVar tPeer peer { pBlocks = Nothing }
        return $ print "Piece complete!"
    | otherwise = retry
        -- unsafeIOToSTM $ print $ "Not complete piece: " ++ countDL pBlocks ++ "/" ++ countAll pBlocks
  where
    allBlocksDLed (Just blocks) = and $ fmap isDownloaded blocks
    allBlocksDLed Nothing       = False

    packBlocks = B.concat . fmap 
        (\BlockInfo {binfoStatus = BlockDownloaded dl} -> dl)

    countDL (Just blocks)  = show $ Seq.length $ Seq.filter id $ fmap isDownloaded blocks
    countDL Nothing        = "NA"
    countAll (Just blocks) = show $ Seq.length blocks
    countAll Nothing       = "NA"

---------------------------------------------------------

sendResponse :: Peer -> TVar Peer -> Handle -> STM(IO())
sendResponse peer@(Peer {..}) tPeer handle = do
    case Seq.viewl pReqsFrom of
        req@(Block {..}) :< _ -> do
            writeTVar tPeer peer { pReqsFrom = Seq.filter (/=req) $ pReqsFrom }
            return $ sendMsg handle $ BlockMsg { blockIndex   = reqIndex
                                               , blockBegin   = reqBegin
                                               , blockContent = B.replicate reqLength (1 :: Word8) }
        _                     -> retry

---------------------------------------------------------

-- Listens to incoming messages and changes state/responds to message.
peerListener :: TVar Peer -> Handle -> IO ()
peerListener tPeer handle = forever $ do
    msg <- getMsg
    case msg of
        BlockMsg {..} -> printf "LISTENER: Received Piece of (index, begin): %s %s\n" (show blockIndex) (show blockBegin)
        _             -> printf "LISTENER: %s\n" (show msg)
    case msg of
        KeepAlive           -> return ()
        Choke               -> updatePeer (\p -> p { pChokingMe = True })
        Unchoke             -> updatePeer (\p -> p { pChokingMe = False })
        Interested          -> updatePeer (\p -> p { pInterestedMe = True })
        Uninterested        -> updatePeer (\p -> p { pInterestedMe = False })
        -- TODO: Add bitfield validation
        Bitfield ps         -> updatePeer (\p -> p { pPeerPieces = ps })
        Have ind            -> updatePeer (\p@(Peer {..}) -> p { pPeerPieces = Seq.update ind True pPeerPieces })
        Request req         -> updatePeer (\p@(Peer {..}) -> p { pReqsFrom = pReqsFrom |> req })
        block@BlockMsg {..} -> updatePiece block tPeer
        Cancel req          -> do
            let filterReqsFrom p@(Peer {..}) = p { pReqsFrom = Seq.filter (req /=) pReqsFrom }
            updatePeer filterReqsFrom
        Port _              -> return ()
  where
    updatePeer = atomically . modifyTVar tPeer
    getMsg = do
        len <- B.hGet handle 4
        let intLen = fromIntegral (BIN.decode $ L.fromStrict len :: Word32)
        msg <- B.hGet handle intLen
        return . decodeMsg $ len <> msg

-- get block, saves block
updatePiece :: PeerMessage -> TVar Peer -> IO ()
updatePiece (BlockMsg {..}) tPeer = atomically $ modifyTVar tPeer updateState
  where
    updateState :: Peer -> Peer
    updateState p@Peer {..} =
        let recBlock = Block { reqIndex  = blockIndex
                             , reqBegin  = blockBegin
                             , reqLength = B.length blockContent }
            isDifferentBlock (_, reqBlock) = reqBlock /= recBlock
        in p { pReqsTo = Seq.filter isDifferentBlock pReqsTo
             , pBlocks = pBlocks `addBlock` blockContent }

    blockNumber = blockBegin `div` 16384 

    addBlock :: Maybe (Seq BlockInfo) -> ByteString -> Maybe (Seq BlockInfo)
    Nothing     `addBlock` _     = Nothing
    Just blocks `addBlock` block = Just $
        Seq.adjust (\b -> b { binfoStatus = BlockDownloaded block }) blockNumber blocks

---------------------------------------------------------

{- HANDSHAKE FUNCTIONS -}

peerHandshake :: Handle -> ByteString -> ByteString -> Maybe ByteString -> IO ()
peerHandshake handle infohash peerid trackerid = do
    handshakeToPeer handle $ formHandshake infohash peerid
    peerhs@Handshake {..} <- handshakeFromPeer handle
    case validateHandshake peerhs (fromMaybe hsPeerId trackerid) infohash of
        Left s    -> fail $ "invalid handshake: " ++ s
        _         -> return ()

handshakeToPeer :: Handle -> Handshake -> IO ()
handshakeToPeer handle handshake = B.hPut handle $ encodeHandshake handshake

handshakeFromPeer :: Handle -> IO Handshake
handshakeFromPeer handle = do
    pstrlen <- L.hGet handle 1
    let intPstrlen = fromIntegral $ L.head pstrlen
    peerhs <- L.hGet handle $ intPstrlen + 48
    return . decodeHandshake . L.toStrict $ pstrlen <> peerhs

validateHandshake :: Handshake -> ByteString -> ByteString -> Either String ()
validateHandshake (Handshake {..}) expectedPeerId infohash
    | hsProtocolId /= "BitTorrent protocol" = Left "Peer not using 'BitTorrent protocol'."
    | hsInfoHash /= infohash                = Left "Peer seeking handshake for wrong file."
    | hsPeerId /= expectedPeerId            = Left "Peer ID doesn't match that reported by tracker."
    | otherwise                             = Right ()

---------------------------------------------------------

{- SUPPORT FUNCTIONS -}

isUnclaimed :: PieceInfo -> Bool
isUnclaimed PieceInfo {pinfoStatus = Unclaimed} = True
isUnclaimed _                                   = False

isUnrequested :: BlockInfo -> Bool
isUnrequested BlockInfo {binfoStatus = BlockUnrequested} = True
isUnrequested _                                          = False

isDownloaded :: BlockInfo -> Bool
isDownloaded BlockInfo {binfoStatus = BlockDownloaded _} = True
isDownloaded _                                           = False

switchPieceSt :: PieceStatus -> PieceID -> GlobalPieces -> GlobalPieces
switchPieceSt st pieceid = Seq.adjust (\p -> p { pinfoStatus = st }) pieceid

switchBlockSt :: BlockStatus -> Int -> Seq BlockInfo -> Seq BlockInfo
switchBlockSt st blockid = Seq.adjust (\p -> p { binfoStatus = st }) blockid
