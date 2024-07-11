module Main where

import Lib.AST.Type
import Lib.Parser

main :: IO ()
main = do
  print "hello hlinter"
  print $ runParser pType "bytes2 x"
