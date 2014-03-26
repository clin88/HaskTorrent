{-# LANGUAGE TupleSections, DeriveDataTypeable #-}

module PieceFile
( Mode(..)
, PieceFile(), pieceFile
, closeET, close
,  readET, read
, writeET, write
) where

-- todo rename ErrorT functions to reflect their type
-- add functions with IO type
-- put the filename into pieceinfo

-- Not threadsafe.
-- Recommended import:
--import qualified PieceFile as PieceFile
--import PieceFile (PieceFile, pieceFile)

-- qualified

import qualified System.IO.MMap as MMap
import qualified Foreign.Marshal.Utils as M

-- mixed

import qualified Data.IORef as IORef
import Data.IORef (IORef)

import qualified Control.Exception as Exc
import Control.Exception (Exception)

import qualified PieceInfo as PI
import PieceInfo (PieceInfo, PieceID)

import qualified Foreign.Ptr as P
import Foreign.Ptr (Ptr)

import qualified Data.ByteString as B
import Data.ByteString (ByteString)

-- unqualified

import Prelude hiding (read)
import Text.Printf (printf)
import Foreign.C.String (CStringLen)
import Foreign.C.Types (CChar)
import Control.Monad.Error (ErrorT, Error, liftIO, throwError, forM_, unless, runErrorT)
import Data.Typeable (Typeable)

type PieceFilePtr = Ptr CChar
type PieceFileState = (PieceFilePtr, IO ())
data PieceFile = PieceFile
    { pfInfo :: PieceInfo
    , pfRefMInfo :: IORef (Maybe PieceFileState)
    -- (mmap'd buffer, action to unmap the buffer)
    }

-- Open a file.
pieceFile :: FilePath -> Mode -> PieceInfo -> IO PieceFile
pieceFile path mode info = do
    -- mmap the path from zero to the file's length
    (ptr, rawsize, offset, size) <- MMap.mmapFilePtr path
                                                     (mmapMode mode)
                                                     (Just (0, PI.piFileSize info))
    -- make an ioref containing the buffer pointer and unmap function
    rmstate <- IORef.newIORef $ Just ( ptr
                                     , MMap.munmapFilePtr ptr rawsize )
    -- construct output
    return PieceFile { pfInfo = info
                     , pfRefMInfo = rmstate }

getState :: PieceFile -> PieceID -> String -> ErrorT String IO (PieceFilePtr, PI.FileOffset, PI.PieceSize)
getState handle pid who = do
    -- extract the pointer
    m <- liftIO $ IORef.readIORef (pfRefMInfo handle)
    (ptr, _) <- throwMaybe (printf "PieceFile.%s: already closed" who) m
    -- extract piece stats
    (offset, size) <- throwMaybe (printf "PieceFile.%s: PieceID(%d) out of range" who pid)
        $ PI.pidBlock (pfInfo handle) pid
    return (ptr, offset, size)

-- Close an open file.
closeET :: PieceFile -> ErrorT String IO ()
closeET handle = do
    -- replace the state with nothing
    m <- liftIO $ IORef.atomicModifyIORef (pfRefMInfo handle) (Nothing,)
    -- run the unmap action
    (_, unmap) <- throwMaybe "PieceFile.close: already closed" m
    liftIO unmap

-- Read a bytestring from a file.
readET :: PieceFile -> PieceID -> ErrorT String IO ByteString
readET handle pid = do
    (ptr, offset, size) <- getState handle pid "readET"
    -- spoof a cstring
    liftIO . B.packCStringLen $ (ptr `P.plusPtr` offset, size)

-- Write a bytestring into a piece in the file.
writeET :: PieceFile -> PieceID -> ByteString -> ErrorT String IO ()
writeET handle pid bytes = do
    (ptr, offset, size) <- getState handle pid "writeET"
    -- make sure input is the right size
    unless (B.length bytes == size)
        $ throwError (printf "PieceFile.write: bytestring(%d) wrong size for piece#%d(%d)" (B.length bytes) pid size)
    -- copy a cstring into the file
    liftIO . B.useAsCStringLen bytes $ \(cstr, _) ->
        M.copyBytes (ptr `P.plusPtr` offset) cstr size

-- opening modes for a PieceFile

data Mode = Create
          | Resume

mmapMode :: Mode -> MMap.Mode
mmapMode Create = MMap.ReadWriteEx
mmapMode Resume = MMap.ReadWrite

-- functions to convert Either Maybe into ErrorT

throwEither :: Error a => Either a b -> ErrorT a IO b
throwEither = either throwError return

throwMaybe :: Error a => a -> Maybe b -> ErrorT a IO b
throwMaybe s = maybe (throwError s) return

-- function to convert an ErrorT into a normal IO action which throws exceptions

newtype PieceFileError = PieceFileError String deriving (Typeable, Show)
instance Exception PieceFileError

throwErrorT :: ErrorT String IO b -> IO b
throwErrorT action = do
    result <- runErrorT action
    either (Exc.throwIO . PieceFileError) return result

close :: PieceFile -> IO ()
close = throwErrorT . closeET

read :: PieceFile -> PieceID -> IO ByteString
read handle pid = throwErrorT $ readET handle pid

write :: PieceFile -> PieceID -> ByteString -> IO ()
write handle pid bytes = throwErrorT $ writeET handle pid bytes

-- eof
