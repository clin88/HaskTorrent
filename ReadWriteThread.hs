module ReadWriteThread where

-- qualified

import qualified Data.Traversable as Trav
import qualified Control.Exception as Exc
import qualified System.FilePath.Posix as Path
import qualified System.Directory as Dir
import qualified Control.Monad as M

-- mixed

import qualified Data.Map.Lazy as Map
import Data.Map.Lazy (Map)

import qualified Data.ByteString as B
import Data.ByteString (ByteString)

import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM (TChan)

import qualified PieceFile
import PieceFile (PieceFile, pieceFile)

-- unqualified

import Control.Concurrent.Async (Async, async)
import PieceInfo (PieceInfo, PieceID)
import Data.Maybe (fromMaybe)
import Text.Printf (printf)

-- data definitions

type Files = Map FilePath (PieceFile.Mode, PieceInfo)
type PieceFiles = Map FilePath PieceFile

type RequestChan = TChan Request
type ResponseChan = TChan ByteString

data Request = Read FilePath PieceID ResponseChan
             | Write FilePath PieceID ByteString

type Summary = ()

-- initialization

launch :: Files -> IO (RequestChan, Async Summary)
launch files = do
    reqs <- STM.newTChanIO
    thread <- async (begin files reqs)
    return (reqs, thread)

-- run loop with the open piecefiles, printing and re-raising any exceptions that occur in loop
begin :: Files -> RequestChan -> IO Summary
begin files reqs = Exc.bracket (Trav.sequence $ Map.mapWithKey open files)
                               (\handles -> do printf "R/W thread: cleaning up\n"
                                               Trav.mapM PieceFile.close handles)
                               (loop reqs)
    where
        -- open one piecefile
        open path (mode, info) = do
            -- mkdir -p if we are to create a new file
            M.when (mode == PieceFile.Create) $ do
                let (dirp, _) = Path.splitFileName path
                Dir.createDirectoryIfMissing True dirp
            pieceFile path mode info

-- mainloop

loop :: TChan Request -> PieceFiles -> IO Summary
loop reqs handles = do
    -- reading reqs TChan and writing resp TChan do not have to be atomic with
    -- respect to each other because from this thread's perspective both are
    -- one-way
    --      reqs is blocking read-only
    --      resp is nonblocking write-only
    req <- STM.atomically . STM.readTChan $ reqs
    case req of
        Write p pid bytes -> do
            PieceFile.write (getpf p) pid bytes
            printf "R/W thread: wrote %d bytes to %s\n" (B.length bytes) p
        Read p pid resp -> do
            bytes <- PieceFile.read (getpf p) pid
            STM.atomically . STM.writeTChan resp $! bytes
            printf "R/W thread: read %d bytes from %s\n" (B.length bytes) p
    loop reqs handles
    where
        getpf :: FilePath -> PieceFile
        getpf p = fromMaybe (error $ printf "R/W thread: no such file %s" p)
                            (Map.lookup p handles)

-- eof
