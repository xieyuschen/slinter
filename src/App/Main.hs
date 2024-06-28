module Main where

import Lib.Parser
import Lib.AST

main :: IO ()
main = do
  print "hello hlinter"
  let input = "pragma solidity 0.8.24;"
  print $ runParser pPragma input
