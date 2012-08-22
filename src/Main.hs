{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

import qualified Data.ByteString.Lazy as BS

import Operation
import Machine
import Template

operation = [brainfuck|
>+++++++++[<++++++++>-]<.>+++++++[<++++>-]<+.+++++++..+++.[-]>++++++++[<++
++>-]<.>+++++++++++[<+++++>-]<.>++++++++[<+++>-]<.+++.------.--------.[-]>
++++++++[<++++>-]<+.[-]++++++++++.
|]

main =
  BS.putStr $ exec $ read operation
