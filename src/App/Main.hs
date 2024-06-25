module Main where
import Lib.Lexier (scanMultipleWords, evalScanner)

main :: IO ()
main = do
  let input = "foo = 42 + bar"
  foldMap print $ evalScanner (scanMultipleWords 1) input
  