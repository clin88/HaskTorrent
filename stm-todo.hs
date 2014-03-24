-- ? main might want to undo our claim ?
-- ? claim reliquishment if we aren't actively downloading ?

-- pieceID
-- blockLen
-- state of blocks in current piece
data Blocks = IntMap Bool
-- pending requests
data RequestQ = Seq (BlockOffset, BlockSize)

-- makes sure this peer thread has a claim
-- sends finished downloads to writer
pieceManager :: STM (IO ())
-- check block status, if all present
--      package for writer
--      change claim->downloaded
--      get new claim
-- else
--      retry

-- makes sure we have 10 pending requests from peer
sendReq :: STM (IO ())
-- if size.reqq<10
--      make
--      send messages
--      update q

sendHaves -- done

gaugeInterest
-- Interested (They have something) -> Interested
--   0          1                      1
--   1          0                      0
--   1          1                      -
--   0          0                      -

recvPiece :: L
-- update block status
-- remove corresponding request from q
