{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Template
  ( brainfuck
  , brainfuckQ
  ) where

import qualified Data.ByteString.Char8 as BS
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import System.Process
import Text.Parsec

import Operation

brainfuck :: QuasiQuoter
brainfuck = QuasiQuoter
  { quoteExp = brainfuckQ
  }

brainfuckQ :: String -> Q Exp
brainfuckQ rawStr = do
  operations <- runIO $ do
    op <- readProcess "./Parser" [] rawStr
    return op
  [e| operations |]
