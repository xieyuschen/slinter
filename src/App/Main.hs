module Main where

import Lib.AST
import Lib.Parser
import Text.Read (Lexeme(String))
import Control.Applicative ((<|>))

main :: IO ()
main = do
  print "hello hlinter"
  let str = " ( uint256 name) "
  print $ runParser pFunctionArgsQuoted str