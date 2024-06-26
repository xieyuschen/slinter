{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "[]" #-}
module Lib.Lexier where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Control.Monad.State
    ( evalState, runState, MonadState(state), State, StateT(..) )
import Control.Monad.Reader ( runReader, Reader, ReaderT(ReaderT) )
import Data.Text (breakOn)
import Data.List (find)
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExcept, runExceptT)
import Text.Read (Lexeme(String), readMaybe)
import Control.Monad.Except
import Control.Arrow (ArrowChoice(right))

data Token = Word String
           | Number Int
           | Whitespace
           | Abnormal
           deriving (Show, Eq)

type Consumer = ExceptT String (State String) Token

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail xs = Just $ tail xs

consumeSpace :: Consumer
consumeSpace = ExceptT $ state $ \s -> do
  if head s == ' ' then (Right Whitespace, tail s) else
    (Left "", s)


consumeWord :: Consumer
consumeWord = ExceptT $ state $ \s -> do
  case safeTail s of
    Nothing -> (Left "cannot find the right \"", s)
    Just xs -> do
      (if head s /= '\"' then (Left "not a string", s) else (do
        let (word, left) = break (=='\"') xs -- remove the leading "
        if left == "" || head left /= '\"'
          then (Left "missing right quote", s)
          else do
          (Right $ Word $ "\"" ++ word ++ "\"", tail left))) -- remove the ending "

consumeNum :: Consumer
consumeNum = ExceptT $ state $ \s-> do
  let (num,left) = span isDigit s
  if num=="" then (Left "", "") else
    case readMaybe num of
      Nothing -> (Left $ "fail to parse num: " ++ num, "")
      Just n -> (Right $ Number n,left)

-- each consumer must return a correct anwser, otherwise we will return due to an error
extract :: [Consumer] -> ExceptT String (State String) [Token]
extract predicateActions = ExceptT $ state $ \s -> do
    foldr f (Right [], s) predicateActions where
      f :: Consumer -> (Either String [Token], String) -> (Either String [Token], String)
      f predicateAction (ei,s') = do
        case ei of
          Left str -> (Left str, s')
          Right tokens -> do
            if s' == "" then (ei, s') else do
              let (tokenE, s'') = runState (runExceptT predicateAction) s'
              case tokenE of
                Left str -> (Left str,s'')
                Right ntoken -> do
                  (Right $ tokens ++ [ntoken], s'')

scanOneSt :: Consumer -> String -> (Either String [Token], String)
scanOneSt consumer = runState (runExceptT $ extract [consumer])


scanMultipleSt :: Consumer -> String -> Int -> (Either String [Token], String)
scanMultipleSt consumer s num
  | num == 0 = (Right [], s)
  | otherwise = do
    let (token, s') = scanOneSt consumer s
        (ntoken, s'') = scanMultipleSt consumer s' $ num-1
    case (++) <$> token <*> ntoken of
      Left str -> (Left str, s'')
      Right tokens -> (Right tokens, s'')

scanAllSt ::  Consumer -> String -> Either String [Token]
scanAllSt c s
  | s == "" = Right []
  | otherwise = do
    let (tokens, s') =  scanOneSt c s
        ntokens = scanAllSt c s'
    (++) <$> tokens <*> ntokens

scanOne :: Consumer -> String -> Either String [Token]
scanOne consumer = evalState (runExceptT $ extract [consumer])

scanMultiple :: Consumer -> String -> Int -> Either String [Token]
scanMultiple c s n = do
  let (tokens, _) = scanMultipleSt c s n
  tokens