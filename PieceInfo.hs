module PieceInfo
-- File pieces
( FileSize
, FileOffset
, PieceID
, PieceSize
, PieceCount
, PieceInfo(), pieceInfo
, pidInRange
, pidIsLast
, pidSize
, pidOffset
, pidBlock
, pidBlockInfo 
-- Piece blocks
, BlockID
, BlockSize, blockSize
, PieceOffset
, BlockInfo(), blockInfo
, bidBlock
) where

import qualified Control.Monad as Monad

-- Create a PieceInfo for each file.
-- Query it with PieceIDs to access file location stats.

type FileSize = Int
type FileOffset = Int

type PieceID = Int
type PieceSize = Int
type PieceCount = Int

data PieceInfo = PieceInfo
    { piFileSize :: FileSize
    , piPieceSize :: PieceSize
    , piLastPieceSize :: PieceSize
    , piPieceCount :: PieceCount
    } deriving (Show)

pieceInfo :: FileSize -> PieceSize -> PieceInfo
pieceInfo fileSize pieceSize = PieceInfo
    { piFileSize = fileSize
    , piPieceSize = pieceSize
    , piLastPieceSize = if aligned then pieceSize
                                   else lastPieceSize
    , piPieceCount = if aligned then wholePieceCount
                                else wholePieceCount + 1
    }
    where
        (wholePieceCount, lastPieceSize) = fileSize `quotRem` pieceSize
        aligned = 0 == lastPieceSize

pidInRange :: PieceInfo -> PieceID -> Bool
pidInRange pinf pid = pid >= 0 && pid < piPieceCount pinf

pidIsLast :: PieceInfo -> PieceID -> Maybe Bool
pidIsLast pinf pid = do Monad.guard $ pidInRange pinf pid
                        return $ pid == (piPieceCount pinf - 1)

pidSize :: PieceInfo -> PieceID -> Maybe PieceSize
pidSize pinf pid = do isLast <- pidIsLast pinf pid
                      return $ if isLast then piLastPieceSize pinf
                                         else piPieceSize pinf

pidOffset :: PieceInfo -> PieceID -> Maybe FileOffset
pidOffset pinf pid = do Monad.guard $ pidInRange pinf pid
                        return $ pid * piPieceSize pinf

pidBlock :: PieceInfo -> PieceID -> Maybe (FileOffset, PieceSize)
pidBlock pinf pid = do offset <- pidOffset pinf pid
                       size <- pidSize pinf pid
                       return (offset, size)

pidBlockInfo :: PieceInfo -> PieceID -> Maybe BlockInfo
pidBlockInfo pinf pid = do (_, size) <- pidBlock pinf pid
                           return $ blockInfo size

-- Instantiate a BlockInfo for a piece, and use it to get block info.

type BlockID = PieceID
type BlockSize = PieceSize
type PieceOffset = FileOffset

newtype BlockInfo = BlockInfo PieceInfo
    deriving (Show)

blockSize :: BlockSize
blockSize = 2 ^ (10::Int) * 16 -- 16KiB

-- Reuse the PieceInfo code
-- The "file" is actually a piece now and the "piece" is a block now
blockInfo :: PieceSize -> BlockInfo
blockInfo pieceSize = BlockInfo $ pieceInfo pieceSize blockSize

bidBlock :: BlockInfo -> BlockID -> Maybe (PieceOffset, BlockSize)
bidBlock (BlockInfo binf) bid = pidBlock binf bid

-- eof
