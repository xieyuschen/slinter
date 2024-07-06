{-# LANGUAGE DoAndIfThenElse #-}

module Lib.Parser where

import Control.Applicative (Alternative ((<|>)), liftA2, liftA3)
import Control.Arrow (first)
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
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import Data.Char (isAlpha, isAlphaNum, isDigit, isNumber, isSpace)
import Data.List (stripPrefix)
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
    "" -> first Right (s, "") -- only one line, so we return all as this line
    _ -> first Right (left, tail right)

pUntil :: (Char -> Bool) -> Parser String
pUntil cond = ExceptT $ state $ \s -> do
  let (left, right) = break cond s
  case right of
    "" -> first Left ("cannot find the until char", s)
    _ -> first Right (left, right)

-- 0 or more times
pMany :: Parser a -> Parser [a]
pMany p = pMany1 p <|> return []

-- 0 or 1 time
pOpt :: Parser a -> Parser (Maybe a)
pOpt p = ExceptT $ state $ \s -> do
  let (result, s') = runParser p s
  case result of
    Left msg -> first Right (Nothing, s)
    Right d -> first Right (Just d, s')

-- 1 or more times
pMany1 :: Parser a -> Parser [a]
pMany1 p = liftA2 (:) p (pMany p)

pManyStop :: (Eq a) => Parser a -> a -> Parser [a]
pManyStop p v = ExceptT $ state $ \s -> do
  let (result, s') = runParser p s
  case result of
    Left msg -> first Right ([], s)
    Right d ->
      if d == v
        then first Right ([], s)
        else do
          let (r, s'') = runParser (pManyStop p v) s'
          (fmap (d :) r, s'')

pMany1Stop :: (Eq a) => Parser a -> a -> Parser [a]
pMany1Stop p v = ExceptT $ state $ \s -> do
  let (result, s') = runParser p s
  case result of
    Left msg -> (Left msg, s)
    Right d ->
      if d == v
        then first Right ([], s)
        else do
          let (r, s'') = runParser (pManyStop p v) s'
          (fmap (d :) r, s'')

-- one creates a parser to consume the given string
-- and generate a type based on the passed constructor
pOne :: String -> (String -> a) -> Parser a
pOne desired f = ExceptT $ state $ \s -> do
  case stripPrefix desired s of
    Nothing -> first Left ("", s)
    Just remainder -> first Right (f desired, remainder)

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail xs = Just $ tail xs

pManySpaces :: Parser [()]
pManySpaces = pMany pSpace

pOneKeyword :: String -> Parser String
pOneKeyword s = pOne s id

pSpace :: Parser ()
pSpace = ExceptT $ state $ \s -> do
  if s /= "" && (head s == ' ' || head s == '\n')
    then first Right ((), tail s)
    else
      first Left ("", s)

-- todo: support unicode and hex, such as:
-- unicode"Hello 😃";
-- hex"0011223344556677"
pString :: Parser String
pString = ExceptT $ state $ \s -> do
  case safeTail s of
    Nothing -> first Left ("cannot find the right \"", s)
    Just xs -> do
      ( if head s /= '\"'
          then first Left ("not a string", s)
          else
            ( do
                let (word, left) = break (== '\"') xs -- remove the leading "
                if left == "" || head left /= '\"'
                  then first Left ("missing right quote", s)
                  else do
                    first Right ("\"" ++ word ++ "\"", tail left) -- remove the ending "
            )
        )

pNumber :: Parser Int
pNumber = ExceptT $ state $ \s -> do
  let (num, left) = span isDigit s
  if num == ""
    then first Left ("", s)
    else
      first Right (read num, left)

isUnderscore :: Char -> Bool
isUnderscore = (== '_')

pIdentifier :: Parser String
pIdentifier = ExceptT $ state $ \s ->
  let base = liftA2 (||) isAlpha isUnderscore
      (identifier, rest) = span (liftA2 (||) isNumber base) s
   in if not (null identifier) && base (head identifier)
        then first Right (identifier, rest)
        else first Left ("Failed to parse identifier", s)

pSemVer :: Parser SemVer
pSemVer = do
  _ <- pManySpaces
  rangeC <- pOneKeyword "^" <|> pOneKeyword "~" <|> pOneKeyword "*" <|> pure ""
  let char = case rangeC of
        "^" -> Just Caret
        "~" -> Just Tilde
        "*" -> Just Wildcards
        _ -> Nothing
  case char of
    Just Wildcards ->
      return
        ( SemVer
            { semVerRangeMark = Just Wildcards,
              major = 0,
              minor = 0,
              patch = Nothing
            }
        )
    _ -> do
      major <- pNumber
      _ <- pOneKeyword "."
      minor <- pNumber
      _ <- pOpt (pOneKeyword ".")
      patch <- pOpt pNumber -- todo: refine me
      return (SemVer {major = major, minor = minor, patch = patch, semVerRangeMark = char})
