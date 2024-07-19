-- todo: can we remove it?
{-# LANGUAGE DoAndIfThenElse #-}

module Lib.Parser where

import Control.Applicative
  ( Alternative ((<|>)),
    Applicative (liftA2),
    optional,
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
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR

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

type ErrMsg = Text

-- pass the type you want to pass, so use a instead of [a]
type Parser a = ExceptT ErrMsg (State Text) a

runParser :: Parser a -> Text -> (Either Text a, Text)
runParser p = runState (runExceptT p)

-- pTry runs the parser, if it fails it restores the state consumed by the parser
pTry :: Parser a -> Parser a
pTry p = ExceptT $ state $ \s -> do
  let (result, s') = runParser p s
  case result of
    -- we cut the error message as well, because sometimes we use <|> to append computations
    Left _ -> first Left ("", s)
    Right r -> first Right (r, s')

pReadline :: Parser Text
pReadline = ExceptT $ state $ \s -> do
  let (left, right) = T.break (== '\n') s
  case right of
    "" -> first Right (s, "") -- only one line, so we return all as this line
    _ -> first Right (left, T.tail right)

pUntil :: (Char -> Bool) -> Parser Text
pUntil cond = ExceptT $ state $ \s -> do
  let (left, right) = T.break cond s
  case right of
    "" -> first Left ("cannot find the until char", s)
    _ -> first Right (left, right)

-- 0 or more times
many :: Parser a -> Parser [a]
many p = pMany1 p <|> return []

-- 1 or more times
pMany1 :: Parser a -> Parser [a]
pMany1 p = liftA2 (:) p (many p)

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

pOne :: (Char -> Bool) -> Parser Text
pOne f = do
  s <- get
  let (prefix, s') = T.span f s
  guard $ not $ T.null prefix
  put s' >> return prefix

-- one creates a parser to consume the given string
-- and generate a type based on the passed constructor
pStringTo :: Text -> (Text -> a) -> Parser a
pStringTo desired f = ExceptT $ state $ \s -> do
  case T.stripPrefix desired s of
    Nothing -> first Left ("fail to find desired charactor: '" <> desired <> "';", s)
    Just remainder -> first Right (f desired, remainder)

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail xs = Just $ tail xs

pManySpaces :: Parser [()]
pManySpaces = many pSpace

pOneKeyword :: Text -> Parser Text
pOneKeyword s = pStringTo s id

pSpace :: Parser ()
pSpace = ExceptT $ state $ \s -> do
  if s /= "" && (T.head s == ' ' || T.head s == '\n')
    then first Right ((), T.tail s)
    else
      first Left ("fail to parse a space", s)

-- todo: support unicode and hex, such as:
-- unicode"Hello 😃";
-- hex"0011223344556677"
pString :: Parser Text
pString = do
  s <- get
  guard $ not (T.null s)
  let xs = T.tail s
  guard $ T.head s /= '\"'
  let (word, left) = T.break (== '\"') xs -- remove the leading "
  if left == "" || T.head left /= '\"'
    then throwError "missing right quote"
    else do
      put (T.tail left) >> return ("\"" <> word <> "\"")

pNumber :: Parser Int
pNumber = ExceptT $ state $ \s -> do
  let (num, left) = T.span isDigit s
  if num == ""
    then first Left ("", s)
    else
      first Right (read $ T.unpack num, left)

pBool :: Parser Bool
pBool = do
  s <- get
  guard $ (\x -> T.length x >= 4) s
  let s' = T.drop 4 s
  case T.take 4 s of
    "true" -> do
      put s'
      return True
    "fals" -> do
      guard $ not $ T.null s'
      case T.take 1 s' of
        "e" -> do
          put $ T.drop 1 s'
          return False
        _ -> throwError "not a bool literal"
    _ -> throwError "not a bool literal"

isUnderscore :: Char -> Bool
isUnderscore = (== '_')

pIdentifier :: Parser Text
pIdentifier = ExceptT $ state $ \s ->
  let base = liftA2 (||) isAlpha isUnderscore
      (identifier, rest) = T.span (liftA2 (||) isNumber base) s
   in if not (T.null identifier) && base (T.head identifier)
        then first Right (identifier, rest)
        else first Left ("Failed to parse identifier", s)

pSemVer :: Parser SemVer
pSemVer = do
  rangeC <-
    pManySpaces
      >> pOneKeyword "^"
        <|> pOneKeyword "~"
        <|> pOneKeyword "*"
        <|> pure ""
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
      minorV <- pOneKeyword "." >> pNumber
      _ <- optional (pOneKeyword ".")
      patchV <- optional pNumber -- todo: refine me
      return (SemVer {major = majorV, minor = minorV, patch = patchV, semVerRangeMark = char})
