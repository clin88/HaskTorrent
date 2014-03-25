module BTUtils where

import Text.Printf (printf)

logger :: String -> String -> IO ()
logger pid = printf "(%s) %s \n" (show pid) 
