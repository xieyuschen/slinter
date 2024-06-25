module Lib.Lexier where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Control.Monad.State
    ( evalState, runState, MonadState(state), State, StateT(..) )
import Control.Monad.Reader ( runReader, Reader, ReaderT(ReaderT) )
import Data.Text (breakOn)
import Data.List (find)

data Token = Word String
           | Number Int
           | Whitespace
           | Abnormal
           deriving (Show, Eq)

type Consumer = State String Token
type Checker =  Char -> Bool
type Extracter = (Checker, Consumer)

newtype Scanner = Scanner { 
  runScanner :: StateT String (Reader [Extracter]) [Token]
}

consumeSpace :: Consumer
consumeSpace = state $ \s -> (Whitespace, tail s)

isString :: Checker
isString s = s == '\"'

consumeString :: Consumer
consumeString = state $ \s -> do
  let (word, left) = break isString $ tail s -- remove the leading "
  (Word $ "\"" ++ word ++ "\"", tail left) -- remove the ending "

consumeNum :: Consumer
consumeNum = state $ \s-> do
  let (num,left) = span isDigit s
  (Number (read num::Int), left)

applyFirstMatch :: [Extracter] -> State String [Token]
applyFirstMatch predicateActions = state $ \s -> do
    let re = find (\(predicate, _) -> predicate (head s)) predicateActions
    case re of 
      Nothing -> ([Abnormal],"")
      Just (_, extract) -> do
        let (token, s') = runState extract s
        ([token], s')

makeScanner :: Scanner
makeScanner = Scanner $ StateT $ \s ->
  ReaderT $ \extracters -> do
    let st = applyFirstMatch extracters
        result = runState st s
    return result

scanOne :: [Extracter] -> State String [Token]
scanOne extracter = state $ \s -> do
    let r = runStateT (runScanner makeScanner) s
    runReader r extracter

scanMultipleSt :: [Extracter] -> Int -> State String [Token]
scanMultipleSt extracter num
  | num == 0 = state $ \s -> ([], s)
  | otherwise = state $ \s -> do
    let r = runStateT (runScanner makeScanner) s
    let (token, s') = runReader r extracter
        (right, s'') = runState (scanMultipleSt extracter $ num-1) s'
    (token ++ right, s'')

scanAllSt :: [Extracter] -> State String [Token]
scanAllSt extracter = state $ \s ->
  case s of
    "" -> return []
    _ -> do
      let (token, s') = runState (scanOne extracter) s
          (token',s'') = runState (scanAllSt extracter) s' 
      (token ++ token', s'')

scanMultiple :: String -> [Extracter] -> Int -> [Token]
scanMultiple input extracters num = evalState (scanMultipleSt extracters num) input

scanAll :: String -> [Extracter] -> [Token]
scanAll input extracters = evalState (scanAllSt extracters) input