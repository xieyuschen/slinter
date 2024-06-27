{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "[]" #-}
module Lib.Lexier where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Control.Monad.State
    ( evalState, runState, MonadState(state, put), State, StateT(..) )
import Control.Monad.Reader ( runReader, Reader, ReaderT(ReaderT) )
import Data.Text (breakOn)
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExcept, runExceptT)
import Text.Read (Lexeme(String), readMaybe)
import Control.Monad.Except
import Control.Arrow (ArrowChoice(right))
import Control.Applicative (liftA2, Alternative ((<|>)))
import Data.List (stripPrefix)

data Token = Str String
           | Number Int
           | Identifier String
           | Keyword String
           | Operator String
           | Whitespace
           | Abnormal
           deriving (Show, Eq)

-- pass the type you want to pass, so use a instead of [a]
type Parser a = ExceptT String (State String) a

-- then, we need to support a lot of combinaters

parseSkip :: Parser a -> Parser [a]
parseSkip p = ExceptT $ state $ \s -> do
  let (ei, s') = runState (runExceptT p) s
  case ei of
    Left str -> (Left str, s)
    Right e -> (return [], s)

-- 0 or more times
parseMany :: Parser a -> Parser [a]
parseMany p = parseMany1 p <|> (state $ \s -> ([],s))

-- 1 or more times
parseMany1 :: Parser a -> Parser [a]
parseMany1 p = liftA2 (:) p (parseMany p)

-- one creates a parser to consume the given string 
-- and generate a type based on the passed constructor
parseOne :: String -> (String -> a) -> Parser a
parseOne desired f= ExceptT $ state $ \s -> do
  case stripPrefix desired s of
    Nothing -> (Left "", s)
    Just remainder -> (Right $ f desired , remainder)

parseThen :: Parser [a] -> Parser [a] -> Parser [a]
parseThen a b = ExceptT $ state $ \s -> 
  let (resA, s')  = runState (runExceptT a) s
      (resB, s'') = runState (runExceptT b) s'
  in case (resA, resB) of
       (Right d, Right d') -> (Right (d ++ d'), s'')
       (Left err, _)       -> (Left err, s')
       (_, Left err)       -> (Left err, s'')

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail xs = Just $ tail xs

parseSpace :: ExceptT String (State String) Token
parseSpace = ExceptT $ state $ \s -> do
  if s/="" && head s == ' ' then (Right Whitespace, tail s) else
    (Left "", s)

parseString :: ExceptT String (State String) Token
parseString = ExceptT $ state $ \s -> do
  case safeTail s of
    Nothing -> (Left "cannot find the right \"", s)
    Just xs -> do
      (if head s /= '\"' then (Left "not a string", s) else (do
        let (word, left) = break (=='\"') xs -- remove the leading "
        if left == "" || head left /= '\"'
          then (Left "missing right quote", s)
          else do
          (Right $ Str $ "\"" ++ word ++ "\"", tail left))) -- remove the ending "

parseNumber :: ExceptT String (State String) Token
parseNumber = ExceptT $ state $ \s-> do
  let (num,left) = span isDigit s
  if num=="" then (Left "", s) else
    (Right $ Number $ read num,left)

parseIdentifier ::  ExceptT String (State String) Token
parseIdentifier = ExceptT $ state $ \s-> 
  let (identifier, rest) = span isAlphaNum s
  in if not (null identifier) && isAlpha (head identifier)
       then (Right (Identifier identifier), rest)
       else (Left "Failed to parse identifier", s)
