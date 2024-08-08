-- todo: can we remove it?
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Parser where

import Control.Monad (guard)
import Data.Char (chr)
import Data.Text (Text)
import qualified Data.Text as T
import Numeric (readHex)
import Text.Parsec

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

type Parser a = Parsec Text () a

type ParserResult a = Either ParseError a

runSParser :: Parser a -> Text -> (ParserResult a, Text)
runSParser p input =
  case runParser combinedParser () "" input of
    Left err -> (Left err, input) -- Return the original input if parsing fails
    Right (result, remaining) -> (Right result, remaining)
  where
    combinedParser = do
      result <- p
      remaining <- getInput
      return (result, remaining)

pOneKeyword :: Text -> Parser Text
pOneKeyword s = T.pack <$> string (T.unpack s)

pReadline :: Parser Text
pReadline = T.pack <$> manyTill anyChar (newline <|> crlf)

-- consume any Unicode space character, and the control characters \t, \n, \r, \f, \v
pManySpaces :: Parser ()
pManySpaces = skipMany space

pMany1Spaces :: Parser ()
pMany1Spaces = skipMany1 space

hexPairToChar :: String -> Char
hexPairToChar hexPair =
  let [(byte, "")] = readHex hexPair
   in chr byte

hexToString :: String -> String
hexToString [] = []
hexToString (x1 : x2 : xs) = hexPairToChar [x1, x2] : hexToString xs
hexToString _ = error "Invalid hex string"

pString :: Parser Text
pString =
  (pOneKeyword "unicode" >> T.pack <$> (char '"' >> many (noneOf "\"") <* char '"'))
    <|> (pOneKeyword "hex" >> T.pack . hexToString <$> (char '"' >> many (noneOf "\"") <* char '"'))
    <|> T.pack <$> (char '"' >> many (noneOf "\"") <* char '"')

pNumber :: Parser Int
pNumber = read <$> many1 digit

pBool :: Parser Bool
pBool =
  (string "true" >> return True)
    <|> (string "false" >> return False)

isUnderscore :: Char -> Bool
isUnderscore = (== '_')

-- todo: currently the pIdentifier will stop once the character could not be recognized as a vliad char
-- for example variable&abc will stop parsing at '&', think about whether we can improve it or not
pIdentifier :: Parser Text
pIdentifier = do
  first <- letter <|> char '_'
  rest <- many (alphaNum <|> char '_')
  return (T.pack $ first : rest)

pSemVer :: Parser SemVer
pSemVer = do
  rangeC <-
    pManySpaces
      >> ( string "^"
             <|> string "~"
             <|> string "*"
             <|> pure ""
         )
  let c = case rangeC of
        "^" -> Just Caret
        "~" -> Just Tilde
        "*" -> Just Wildcards
        _ -> Nothing
  case c of
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
      minorV <- string "." >> pNumber
      _ <- optional (string ".")
      patchV <- optionMaybe pNumber -- todo: refine me
      return (SemVer {major = majorV, minor = minorV, patch = patchV, semVerRangeMark = c})
