{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Lib.Parser where

import Control.Applicative (Alternative ((<|>)), liftA2)
import Control.Arrow (ArrowChoice (right))
import Control.Monad.Except
import Control.Monad.Reader (MonadReader (ask), Reader, ReaderT (ReaderT), runReader)
import Control.Monad.State
  ( MonadState (put, state),
    State,
    StateT (..),
    evalState,
    runState,
  )
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExcept, runExceptT)
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.List (stripPrefix)
import Data.Text (breakOn)
import Data.Version (Version (Version))
import Text.ParserCombinators.ReadP (manyTill)
import Text.Read (Lexeme (String), get, readMaybe)

-- todo: support version range like 1.2.x, 1.x, 1.2.3 - 2.3.4 later
data SemVerRangeMark
  = Caret -- Caret (^): Allows updates that do not change the left-most non-zero digit.
  | Tilde -- Tilde (~): Allows updates to the most recent patch version within the specified minor version.
  | Wildcards -- Wildcards (*): Allows any version.
  deriving (Show, Eq)

data SemVer = SemVer
  { major :: Int,
    minor :: Int,
    patch :: Maybe Int, -- Maybe is used because the patch version might be omitted
    semVerRangeMark :: Maybe SemVerRangeMark
  }
  deriving (Show, Eq)


type ErrMsg = String
-- pass the type you want to pass, so use a instead of [a]
type Parser a = ExceptT ErrMsg (State String) a

runParser :: Parser a -> String -> (Either ErrMsg a, String)
runParser p = runState (runExceptT p)

pReadline :: Parser String
pReadline = ExceptT $ state $ \s -> do
  let (left, right) = break (== '\n') s
  case right of
    "" -> (Right s, "") -- only one line, so we return all as this line
    _ -> (Right left, tail right)

pUntil :: (Char -> Bool) -> Parser String
pUntil cond = ExceptT $ state $ \s -> do
  let (left, right) = break cond s
  case right of
    "" -> (Left "", s)
    _ -> (Right left, tail right)

-- 0 or more times
pMany :: Parser a -> Parser [a]
pMany p = pMany1 p <|> return []

-- 0 or 1 time
pOpt :: Parser a -> a -> Parser a
pOpt p v = p <|> return v

-- 1 or more times
pMany1 :: Parser a -> Parser [a]
pMany1 p = liftA2 (:) p (pMany p)

-- one creates a parser to consume the given string
-- and generate a type based on the passed constructor
pOne :: String -> (String -> a) -> Parser a
pOne desired f = ExceptT $ state $ \s -> do
  case stripPrefix desired s of
    Nothing -> (Left "", s)
    Just remainder -> (Right $ f desired, remainder)

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail xs = Just $ tail xs

parseManySpaces :: Parser [()]
parseManySpaces = pMany pSpace

pOneKeyword :: String -> Parser String
pOneKeyword s = pOne s id

pSpace :: Parser ()
pSpace = ExceptT $ state $ \s -> do
  if s /= "" && head s == ' '
    then (Right (), tail s)
    else
      (Left "", s)

pString :: Parser String
pString = ExceptT $ state $ \s -> do
  case safeTail s of
    Nothing -> (Left "cannot find the right \"", s)
    Just xs -> do
      ( if head s /= '\"'
          then (Left "not a string", s)
          else
            ( do
                let (word, left) = break (== '\"') xs -- remove the leading "
                if left == "" || head left /= '\"'
                  then (Left "missing right quote", s)
                  else do
                    (Right $ "\"" ++ word ++ "\"", tail left) -- remove the ending "
            )
        )

pNumber :: Parser Int
pNumber = ExceptT $ state $ \s -> do
  let (num, left) = span isDigit s
  if num == ""
    then (Left "", s)
    else
      (Right $ read num, left)

pIdentifier :: Parser String
pIdentifier = ExceptT $ state $ \s ->
  let (identifier, rest) = span isAlphaNum s
   in if not (null identifier) && isAlpha (head identifier)
        then (Right identifier, rest)
        else (Left "Failed to parse identifier", s)

pSemVer :: Parser SemVer
pSemVer = do
  _ <- parseManySpaces
  rangeC <- pOneKeyword "^" <|> pOneKeyword "~" <|> pOneKeyword "*" <|> pure ""
  let char = case rangeC of
        "^" -> Just Caret
        "~" -> Just Tilde
        "*" -> Just Wildcards
        _ -> Nothing
  case char of
    Just Wildcards -> return (SemVer 
      {semVerRangeMark = Just Wildcards, major = 0, minor = 0, patch = Nothing}
      )
    _ -> do
      major <- pNumber
      _ <- pOneKeyword "."
      minor <- pNumber
      _ <- pOpt (pOneKeyword ".") ""
      patch <- pOpt pNumber 0 -- todo: refne me
      return (SemVer {major = major, minor = minor, patch = Just patch, semVerRangeMark = char})