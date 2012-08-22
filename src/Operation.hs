{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Operation
  ( BrainfuckOperation (..)
  , brainfuckParser
  , testOps
  ) where

import Control.Applicative ((<*), (<*>), (*>), (<$>))
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Text.Parsec
import Text.Parsec.String
import Language.Haskell.TH.Syntax

data BrainfuckOperation
  = Next
  | Prev
  | Inc
  | Dec
  | Put
  | Get
  | Brace [BrainfuckOperation]
  deriving (Eq, Show, Read)

instance Lift BrainfuckOperation where
  lift op = [| op |]

instance Lift ParseError where
  lift err = do
    runIO $ print err
    [| err |]

single :: Parser BrainfuckOperation
single = msum [ ( char '>' >> return Next )
              , ( char '<' >> return Prev )
              , ( char '+' >> return Inc  )
              , ( char '-' >> return Dec  )
              , ( char '.' >> return Put  )
              , ( char ',' >> return Get  )
              , brace
              ]

brace :: Parser BrainfuckOperation
brace = Brace <$> do
  { char '['
  ; manyOps
  }
  where
    manyOps = do
      { lookAhead (char ']')
      ; return []
      } <|> do
      { op <- single
      ; ops <- manyOps
      ; return (op : ops)
      } <|> do
      { anyChar
      ; manyOps
      }

brainfuckParser :: Parser [BrainfuckOperation]
brainfuckParser = do
  { eof
  ; return []
  } <|> do
  { op <- single
  ; ops <- brainfuckParser
  ; return (op : ops)
  } <|> do
  { anyChar
  ; brainfuckParser
  }

testOps :: [BrainfuckOperation]
testOps = let Right ops = parse brainfuckParser "" test
          in ops

test = "++++++++++[>++++++++++<-] >++++.+++++++.--------.--."
