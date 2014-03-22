{-# LANGUAGE TupleSections #-}

module PieceWriter
( Mode(..)
, PieceWriter(), pieceWriter
, write
, close
) where

-- Not threadsafe.
-- Recommended import:
--import qualified PieceWriter as PieceWriter
--import PieceWriter (PieceWriter, pieceWriter)

-- qualified

import qualified System.IO.MMap as MMap

-- mixed

import qualified Data.IORef as IORef
import Data.IORef (IORef)

import qualified PieceInfo as PI
import PieceInfo (PieceInfo, PieceID)

import qualified Foreign.Storable as S
import Foreign.Ptr (Ptr)

import qualified Data.ByteString as B
import Data.ByteString (ByteString)

-- unqualified

import Control.Monad.Error (ErrorT, Error, liftIO, throwError, forM_, unless)
import Text.Printf (printf)

type PieceWriterState = (Ptr ByteString, IO ())
data PieceWriter = PieceWriter
    { pwInfo :: PieceInfo
    , pwRMState :: IORef (Maybe PieceWriterState)
    -- (mmap'd buffer, action to unmap the buffer)
    }

-- Open a file.
pieceWriter :: FilePath -> Mode -> PieceInfo -> IO PieceWriter
pieceWriter path mode info = do
    -- mmap the path from zero to the file's length
    (ptr, rawsize, offset, size) <- MMap.mmapFilePtr path
                                                     (mmapMode mode)
                                                     (Just (0, PI.piFileSize info))
    -- make an ioref containing the buffer pointer and unmap function
    rmstate <- IORef.newIORef $ Just ( ptr
                                     , MMap.munmapFilePtr ptr rawsize )
    -- construct output
    return $ PieceWriter { pwInfo = info
                         , pwRMState = rmstate }

-- Close an open file.
close :: PieceWriter -> ErrorT String IO ()
close pw = do
    m <- liftIO $ IORef.atomicModifyIORef (pwRMState pw) (Nothing,)
    (_, unmap) <- throwMaybe "PieceWriter.close: already closed" m
    liftIO unmap -- this isn't threadsafe

-- Write a bytestring into a piece in the file.
write :: PieceWriter -> PieceID -> ByteString -> ErrorT String IO ()
write pw pid bytes = do
    -- extract the pointer
    m <- liftIO $ IORef.readIORef (pwRMState pw)
    (ptr, _) <- throwMaybe "PieceWriter.write: already closed" m
    -- extract piece stats
    (offset, size) <- throwMaybe (printf "PieceWriter.write: PieceID(%d) out of range" pid)
        $ PI.pidBlock (pwInfo pw) pid
    -- make sure input is the right size
    unless (B.length bytes == size)
        $ throwError (printf "PieceWriter.write: bytestring(%d) wrong size for piece#%d(%d)" (B.length bytes) pid size)
    -- write to the pointer
    forM_ (take size [0..]) $ \i -> do
        liftIO $ S.pokeByteOff ptr (offset + i) (B.index bytes i)

data Mode = Create
          | Resume

mmapMode :: Mode -> MMap.Mode
mmapMode Create = MMap.ReadWriteEx
mmapMode Resume = MMap.ReadWrite

throwEither :: Error a => Either a b -> ErrorT a IO b
throwEither = either throwError return

throwMaybe :: Error a => a -> Maybe b -> ErrorT a IO b
throwMaybe s = maybe (throwError s) return

-- eof
