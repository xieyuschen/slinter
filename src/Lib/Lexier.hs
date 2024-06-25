{-# LANGUAGE InstanceSigs #-}
module Lib.Lexier where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Control.Monad.State
import Data.Text.Internal.Fusion.Types (Scan)
import Control.Monad.Reader

data Token = Word String
           | Number Int
           deriving (Show, Eq)

-- Define a Scanner type, which is a function from input string to a list of tokens
newtype Scanner a = Scanner { 
  runScanner :: State String [a]
}

executeScaner :: Scanner a -> String -> ([a], String)
executeScaner = runState . runScanner

evalScanner :: Scanner a -> String -> [a]
evalScanner =  evalState . runScanner

instance Functor Scanner where
  fmap :: (a -> b) -> Scanner a -> Scanner b
  fmap f (Scanner v) = Scanner $ fmap f <$> v

instance Applicative Scanner where
  pure :: a -> Scanner a
  pure x = Scanner $ return [x]
  (<*>) :: Scanner (a -> b) -> Scanner a -> Scanner b
  (Scanner sf) <*> (Scanner sx) = Scanner $ state $ \s ->
    let (fs, s')  = runState sf s
        (xs, s'') = runState sx s'
        -- this doesn't make sense in our scenario 
        -- because we won't have the case of multiple functions inside the array
    in ([head fs x  | x <- xs], s'')

  
scanOneWord :: Scanner String
scanOneWord = Scanner $ state $ \s ->
  let (word, rest) = break isSpace (dropWhile isSpace s)
      remaining = dropWhile isSpace rest
  in ([word], remaining)


scanMultipleWords :: Int -> Scanner String
scanMultipleWords n 
  -- pure [] will return a [""], instead []
  | n == 0 = Scanner $ state $ \s -> ([], s)
  | otherwise = Scanner $ state $ \s -> do
    let (word, s') = runState (runScanner scanOneWord) s
    let (leftWord, s'') = executeScaner (scanMultipleWords $ n-1) s'
    (word++leftWord, s'')

scanAllWords :: Scanner String
scanAllWords = Scanner $ state $ \s ->
  if s == "" then pure []
  else do
    let (word, s') = runState (runScanner scanOneWord) s
        (leftWord, s'') = runState (runScanner scanAllWords) s'
    (word++leftWord, s'')

