import Operation

import System.IO
import Text.Parsec

main = do
  input <- getContents
  print input
  case parse brainfuckParser "" input of
    Left err -> hPrint stderr err
    Right op -> print op
