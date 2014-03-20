{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Peers
    --( PeerAddr (..)
    --, Peer
    --, connectToPeer)
where

import Text.Printf (printf)
import           Control.Concurrent.STM
import           Control.Monad          (forM_, forever, join)
import qualified Data.Binary            as BIN
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as L
import           Data.IntMap.Strict     (IntMap)
import qualified Data.IntMap.Strict     as IM
import           Data.Maybe             (fromMaybe)
import           Data.Monoid            ((<>))
import           Data.Sequence          (Seq, (|>))
import qualified Data.Sequence          as Seq
import           Data.IntSet               (IntSet, (\\))
import qualified Data.IntSet               as IS
import Data.Set (Set)
import qualified Data.Set as S
import           Data.Word
import           Network                (HostName, PortID (..), connectTo)
import           PeerMsgs
import           System.IO              (Handle, hSetBinaryMode)
import qualified Data.Time.Clock as Clock
import Data.Time.Clock (UTCTime, NominalDiffTime, diffUTCTime)
import BTUtils

data PeerAddr = PeerAddr
    { peerHost      :: HostName
    , peerPort      :: PortID
    , peerTrackerId :: Maybe ByteString } deriving (Show)

data PieceSt = Claimed
             | Downloaded
             | Unclaimed
             deriving (Show)

type PeerPieces = Seq Bool
type PiecesMap = IntMap PieceSt
type PieceID = Int

data Peer = Peer
    { pAmChoking    :: Bool
    , pAmInterested :: Bool
    , pChokingMe    :: Bool
    , pInterestedMe :: Bool
    , pHasPieces    :: PeerPieces
    , pHaveMapGlob  :: PiecesMap
    , pReqsFrom     :: Seq Block
    , pReqsTo       :: Seq (UTCTime, Block)
    , pCurPiece     :: CurPiece
    }
    -- TODO: Need last send time, last receive time,

data CurPiece = NoPiece
              | CurPiece
              { cpIndex  :: Int
              , cpBlocks :: Set Block
              } deriving (Eq, Show)

defaultPeer :: PiecesMap -> Peer
defaultPeer glPieces = Peer
    { pAmChoking = True
    , pAmInterested = False
    , pChokingMe = True
    , pInterestedMe = False
    , pHasPieces = Seq.empty
    , pHaveMapGlob  = glPieces
    , pReqsFrom = Seq.empty
    , pReqsTo = Seq.empty
    , pCurPiece = NoPiece }

sendMsg :: Handle -> PeerMessage -> IO ()
sendMsg to msg = do
    printf "SENDING MSG: %s\n" (show msg)
    B.hPut to $ encodeMsg msg

peerController :: TVar Peer -> TVar PiecesMap -> Handle -> IO ()
peerController tPeer tPieces handle = forever $ do
    currTime <- Clock.getCurrentTime
    join . atomically $        sendHaves tPeer tPieces handle
                      `orElse` manageInterest tPeer tPieces handle
                      `orElse` reqCleanup tPeer currTime reqExpirationTime
    where
        reqExpirationTime :: NominalDiffTime
        reqExpirationTime = realToFrac 90
    -- make a request
    -- if not choked
    -- and they're interested
    -- and I have this piece
    -- and

    --TODO: Handle request prioritization into just currently claimed pieces.

    --if unchoked ...
    --    if not interested and have interest, express interest, get requests
    --    elif interested but have no interest,
    --    if pending requests are old, delete them
    --    if pending requests < 5 and interested, send request            -- can we guarantee we'll never be interested when there's nothing we need?bou


{- PEERCONTROLLER ACTIONS -}

sendHaves :: TVar Peer -> TVar PiecesMap -> Handle -> STM (IO ())
sendHaves peer glHaves handle = do
    Peer {..} <- readTVar peer
    pHaveMapGlob' <- readTVar glHaves
    case findHaves pHaveMapGlob' pHaveMapGlob  of
        [] -> retry
        xs -> return $ do
            printf "SENDING HAVES: %s\n" (show xs)
            forM_ xs (sendMsg handle . Have)
    where
        findHaves :: PiecesMap -> PiecesMap -> [PieceID]
        findHaves new old = IM.keys $ IM.differenceWith diff new old

        diff :: PieceSt -> PieceSt -> Maybe PieceSt
        diff d@Downloaded Claimed = return d
        diff d@Downloaded Unclaimed = return d
        diff _ _ = Nothing

-- if peer has piece we want, gain interest.
-- if peer has no piece we want, lose interest.
manageInterest :: TVar Peer -> TVar PiecesMap -> Handle -> STM (IO ())
manageInterest tPeer tPieces handle = do
    peer <- readTVar tPeer
    pieces <- readTVar tPieces
    let notInterested = IS.null $ interestedIn pieces (pHasPieces peer)
    case (pAmInterested peer, notInterested) of
        (False, False) -> return $ updatePeer True     -- Not interested now, am interested.
        (True, True)   -> return $ updatePeer False    -- Interested now, not interested.
        _              -> retry
    where
        updatePeer :: Bool -> IO ()
        updatePeer b = do
            atomically . modifyTVar tPeer $ \p -> p {pAmInterested = b}
            sendMsg handle $ if b then Interested
                                  else Uninterested

-- decide what unclaimed pieces we could download from this peer
interestedIn :: PiecesMap -> PeerPieces -> IntSet
interestedIn pieces peerPieces = theyhave \\ ihave
    where
        theyhave :: IntSet
        theyhave = let cmb :: IntSet -> PieceID -> Bool -> IntSet
                       cmb accum ind True = IS.insert ind accum
                       cmb accum _   False = accum
                   in Seq.foldlWithIndex cmb IS.empty peerPieces
        ihave :: IntSet
        ihave = IS.fromList . IM.keys . IM.filter isDownloaded $ pieces
        isDownloaded :: PieceSt -> Bool
        isDownloaded Downloaded = True
        isDownloaded _          = False

reqCleanup :: TVar Peer -> UTCTime -> NominalDiffTime -> STM(IO ())
reqCleanup tPeer currTime expTime = do
    peer@Peer {..} <- readTVar tPeer
    let isFresh (reqTime, _) = currTime `diffUTCTime` reqTime < expTime
        cleanedUpReqs = Seq.filter isFresh pReqsTo
    -- if pieces have been cleaned up, commit the change. Otherwise move on to next STM.
    case cleanedUpReqs /= pReqsTo of
        True  -> return . atomically $ modifyTVar tPeer (\p -> p { pReqsTo = cleanedUpReqs })
        False -> retry

-- Listens to incoming messages and changes state/responds to message.
peerListener :: TVar Peer -> Handle -> IO ()
peerListener tPeer handle = forever $ do
    msg <- getMsg
    printf "LISTENER: %s\n" (show msg)
    case msg of
        KeepAlive        -> return ()
        Choke            -> updatePeer (\p -> p { pChokingMe = True })
        Unchoke          -> updatePeer (\p -> p { pChokingMe = False })
        Interested       -> updatePeer (\p -> p { pInterestedMe = True })
        Uninterested     -> updatePeer (\p -> p { pInterestedMe = False })
        Bitfield ps      -> updatePeer (\p -> p { pHasPieces = ps })
        Have ind         -> updatePeer (\p@(Peer {..}) -> p { pHasPieces = Seq.update ind True pHasPieces })
        Request req      -> updatePeer (\p@(Peer {..}) -> p { pReqsFrom = pReqsFrom |> req } )
        piece@Piece {..} -> updatePiece piece
        Cancel req       -> do
            let filterReqsFrom p@(Peer {..}) = p { pReqsFrom = Seq.filter (req /=) pReqsFrom }
            updatePeer filterReqsFrom
        Port p           -> return ()
    where
        updatePiece Piece {..} = do
            let recBlock = Block { reqIndex  = pieceIndex
                                 , reqBegin  = pieceBegin
                                 , reqLength = B.length pieceBlock }
                isDifferentBlock (_, reqBlock) = reqBlock /= recBlock
                filterReqsTo p@(Peer {..})   = p { pReqsTo = Seq.filter isDifferentBlock pReqsTo }
                                             -- TODO: remove block from curpiece at request time, replace if request expires and not downloaded.
            updatePeer filterReqsTo
        updatePeer = atomically . modifyTVar tPeer
        getMsg = do
            len <- B.hGet handle 4
            let intLen = fromIntegral (BIN.decode $ L.fromStrict len :: Word32)
            msg <- B.hGet handle intLen
            return . decodeMsg $ len <> msg

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
