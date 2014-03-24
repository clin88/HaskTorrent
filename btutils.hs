module BTUtils where

import Text.Printf
import Data.ByteString (ByteString)

logger :: String -> String -> IO ()
logger pid msg = printf "(%s) %s \n" (show pid) msg