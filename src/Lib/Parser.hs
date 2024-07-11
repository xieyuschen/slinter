{-# LANGUAGE DoAndIfThenElse #-}

module Lib.Parser where

import Control.Applicative
  ( Alternative ((<|>)),
    Applicative (liftA2),
  )
import Control.Arrow (Arrow (first))
import Control.Monad.Except
  ( ExceptT (..),
    MonadError (throwError),
    guard,
    runExceptT,
  )
import Control.Monad.State
  ( MonadState (get, put, state),
    State,
    runState,
  )
import Data.Char (isAlpha, isDigit, isNumber)
import Data.List (stripPrefix)

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

-- pTry runs the parser, if it fails it restores the state consumed by the parser
pTry :: Parser a -> Parser a
pTry p = ExceptT $ state $ \s -> do
  let (result, s') = runParser p s
  case result of
    -- we cut the error message as well, because sometimes we use <|> to append computations
    Left msg -> first Left ("", s)
    Right r -> first Right (r, s')

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
    Left _ -> first Right (Nothing, s)
    Right d -> first Right (Just d, s')

-- 1 or more times
pMany1 :: Parser a -> Parser [a]
pMany1 p = liftA2 (:) p (pMany p)

pManyStop :: (Eq a) => Parser a -> a -> Parser [a]
pManyStop p v = ExceptT $ state $ \s -> do
  let (result, s') = runParser p s
  case result of
    Left _ -> first Right ([], s)
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
    Nothing -> first Left ("fail to find desired charactor: '" ++ desired ++ "';", s)
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
      first Left ("fail to parse a space", s)

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

pBool :: Parser Bool
pBool = do
  s <- get
  guard $ (\x -> length x >= 4) s
  let s' = drop 4 s
  case take 4 s of
    "true" -> do
      put s'
      return True
    "fals" -> do
      guard $ not $ null s'
      case take 1 s' of
        "e" -> do
          put $ drop 1 s'
          return False
        _ -> throwError "not a bool literal"
    _ -> throwError "not a bool literal"

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
      majorV <- pNumber
      _ <- pOneKeyword "."
      minorV <- pNumber
      _ <- pOpt (pOneKeyword ".")
      patchV <- pOpt pNumber -- todo: refine me
      return (SemVer {major = majorV, minor = minorV, patch = patchV, semVerRangeMark = char})
