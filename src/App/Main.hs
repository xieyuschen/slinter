module Main where

import Lib.Lexier
import Control.Applicative ((<|>))

main :: IO ()
main = do
  let extracters = consumeWord <|> consumeSpace <|> consumeNum
  let input = "12 34 56"
  print $ scanOne extracters input
  