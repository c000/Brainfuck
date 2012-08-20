{-# LANGUAGE OverloadedStrings #-}
module Operation
  ( BrainfuckOperation (..)
  , brainfuckParser
  , testOps
  ) where

import Control.Applicative ((<*), (<*>), (*>), (<$>))
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Text.Parsec
import Text.Parsec.ByteString

data BrainfuckOperation
  = Next
  | Prev
  | Inc
  | Dec
  | Put
  | Get
  | Brace [BrainfuckOperation]
  deriving (Eq, Show)

single :: Parser BrainfuckOperation
single = msum [ ( char '>' >> return Next )
              , ( char '<' >> return Prev )
              , ( char '+' >> return Inc  )
              , ( char '-' >> return Dec  )
              , ( char '.' >> return Put  )
              , ( char ',' >> return Get  )
              , brace
              , ( noneOf "[]" >> single   )
              ]

brace :: Parser BrainfuckOperation
brace = Brace <$> ( char '[' *> many single <* char ']' )

brainfuckParser = many single <* eof

testOps :: [BrainfuckOperation]
testOps = let Right ops = parse brainfuckParser "" test
          in ops

test = BS.pack "++++++++++[>++++++++++<-] >++++.+++++++.--------.--."
