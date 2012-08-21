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
import Text.Parsec.ByteString
import Language.Haskell.TH.Syntax

data BrainfuckOperation
  = Next
  | Prev
  | Inc
  | Dec
  | Put
  | Get
  | Brace [BrainfuckOperation]
  deriving (Eq, Show)

instance Lift BrainfuckOperation where
  lift op = [| op |]

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
brace = Brace <$> ( char '[' *> many single <* char ']' )

brainfuckParser :: Parser [BrainfuckOperation]
brainfuckParser =
  ( eof >> return [] )
  <|> do
    { skipMany $ notFollowedBy single >> anyChar
    ; op1 <- many single
    ; op2 <- brainfuckParser
    ; return $ op1 ++ op2
    }

testOps :: [BrainfuckOperation]
testOps = let Right ops = parse brainfuckParser "" test
          in ops

test = BS.pack "++++++++++[>++++++++++<-] >++++.+++++++.--------.--."
