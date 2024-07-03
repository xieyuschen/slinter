module Main where

import Lib.AST (pContract)
import Lib.Parser (runParser)

main :: IO ()
main = do
  print "hello hlinter"
  let constractStr =
        "contract Counter { \
        \ uint256 public count;\
        \ // Function to get the current count\
        \function get() public view returns (uint256) {\
        \    return count;\
        \} \n\
        \}"
  print $ runParser pContract constractStr
