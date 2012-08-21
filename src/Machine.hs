module Machine
  ( exec
  , tryExec
  ) where

import qualified Data.ByteString.Lazy as BS
import Data.Word
import Operation

data MachineState = MachineState
  { prev :: [Word8]
  , current :: Word8
  , next :: [Word8]
  , input :: BS.ByteString
  , output :: BS.ByteString
  } deriving Show

printable :: MachineState -> MachineState
printable (MachineState ps c ns i o) = MachineState (take 5 ps) c (take 5 ns) i o

initial i = MachineState (repeat 0) 0 (repeat 0) i BS.empty

step :: BrainfuckOperation -> MachineState -> MachineState
step Next (MachineState ps c (n:ns) i o)= MachineState (c:ps) n ns i o
step Prev (MachineState (p:ps) c ns i o) = MachineState ps p (c:ns) i o
step Inc  (MachineState ps c ns i o) = MachineState ps (c+1) ns i o
step Dec  (MachineState ps c ns i o) = MachineState ps (c-1) ns i o
step Put  m@MachineState { current = c, output = o } = m { output = BS.snoc o c }
step Get  m@MachineState { input = i } = m { current = BS.head i, input = BS.tail i }
step (Brace _) m@(MachineState _ 0 _ _ _) = m
step (Brace ops) m = step (Brace ops) (stepMany ops m)

stepMany :: [BrainfuckOperation] -> MachineState -> MachineState
stepMany ops m = foldl (flip step) m ops

exec :: [BrainfuckOperation] -> BS.ByteString
exec ops = result
  where
    MachineState { output = result } = stepMany ops (initial BS.empty)

tryExec (Left err) = error (show err)
tryExec (Right ops) = result
  where
    MachineState { output = result } = stepMany ops (initial BS.empty)

main = do
  (print . printable) $ stepMany testOps (initial BS.empty)
