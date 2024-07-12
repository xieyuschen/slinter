module Main where

import Lib.AST.Function
import Lib.AST.Type
import Lib.Parser

main :: IO ()
main = do
  print "hello hlinter"
  print $ runParser pType "bytes2 x"
  print $ runParser pFuncCall "set(123+3,456)"
  print $ runParser pFuncCall "example.set( 123+3 , 456+3)"
  print $ runParser pFuncCallArgsList "(123+3,456)"
  print $ runParser pFuncCallArgsNamedParameters "({value: 2+1, key: -3})"
