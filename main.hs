import Data.Maybe (fromJust)
import Control.Monad (liftM, mapM)
import System.Environment (getArgs)
import Data.BEncode as BE (decode, BValue(..), Result)
import Data.BEncode.BDict as BD
import Data.ByteString.Char8 as BS (ByteString, readFile, pack)

getTrackerURL :: BValue -> BValue
getTrackerURL (BDict dict) = fromJust $ BD.lookup (key "announce") dict
    where key = BS.pack

decodeBEncode :: ByteString -> BValue
decodeBEncode bs = either error id $ decode bs

getBTFileName :: IO String
getBTFileName = do
    as <- getArgs
    case as of
        [] -> do print "Specify a bittorrent file please!"; fail "No BT file."
        (x:_) -> return x

main = do
    btdict <- getBTFileName >>= BS.readFile >>= return . decodeBEncode
    tracker <- return $ getTrackerURL btdict
    return ()
