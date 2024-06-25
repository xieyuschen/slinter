module Main where
import Lib.Lexier
import Control.Monad.State

main :: IO ()
main = do
  print $ runState consumeNum "123456"
  