{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Lib.AST.Type where

import Control.Applicative (asum, optional)
import Control.Arrow (Arrow (first))
import Control.Monad.Except
  ( Functor (fmap),
    Monad (return, (>>), (>>=)),
    guard,
  )
import Control.Monad.RWS (MonadReader (ask))
import Control.Monad.State (MonadState (get, put, state))
import Data.Char (isAlpha, isNumber)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text, pack)
import qualified Data.Text as T
import Debug.Trace
import GHC.Base
  ( Applicative (liftA2, (<*)),
    Bool (True),
    Eq ((==)),
    Functor (fmap),
    Int,
    Maybe (..),
    Monad (return, (>>), (>>=)),
    Ord ((<=)),
    const,
    ($),
    (&&),
  )
import Lib.AST.Expr (pExpression)
import Lib.AST.Model
  ( ArrayN (ArrayN, aElemType),
    BitLengthDesc (..),
    Mapping (..),
    SAlias (..),
    SType (..),
    STypeEnum (..),
    Structure (..),
    aSize,
    leftCurlyBrace,
    leftSquareBracket,
    rightSquareBracket,
    semicolon,
  )
import Lib.Parser
  ( Parser,
    isUnderscore,
    pIdentifier,
    pMany1Spaces,
    pManySpaces,
    pNumber,
    pOneKeyword,
    runSParser,
  )
import Text.Parsec
import Text.Read (readMaybe)
import Prelude hiding (foldr)

pType :: Parser SType
pType = do
  pManySpaces
    >> ( STypeMapping <$> try pTypeMapping
           <|> try pTypeArray
           <|> try pTypeSimple
       )

pTypeArray :: Parser SType
pTypeArray = do
  elemTp <- pManySpaces >> pTypeSimple
  brackets <- many1 $ do
    pOneKeyword leftSquareBracket >> optionMaybe pExpression <* pOneKeyword rightSquareBracket

  return $
    foldl
      ( \acc v ->
          STypeArray
            ArrayN
              { aElemType = acc,
                aSize = v
              }
      )
      elemTp
      brackets

-- todo: support me
-- 'mapping(KeyType KeyName? => ValueType ValueName?)'
-- The KeyType can be any built-in value type, bytes, string, or any contract or enum type. O
-- ther user-defined or complex types, such as mappings, structs or array types are not allowed.
-- ValueType can be any type, including mappings, arrays and structs.
-- KeyName and ValueName are optional (so mapping(KeyType => ValueType) works as well)
-- and can be any valid identifier that is not a type.
pTypeMapping :: Parser Mapping
pTypeMapping = do
  keyTyp <-
    pManySpaces
      >> pOneKeyword "mapping("
      >> pManySpaces
      >> pType
        <* pManySpaces

  -- todo: check the key type should be built-in value type, bytes, string, or any contract or enum type
  _ <- optionMaybe pIdentifier -- parse the name, todo: check whether need to store the name
  valTpy <-
    pManySpaces
      >> pOneKeyword "=>"
      >> pManySpaces
      >> pType
        <* pManySpaces
  _ <- optionMaybe pIdentifier >> pOneKeyword ")" >> pManySpaces
  return
    Mapping
      { mKeyType = keyTyp,
        mValueType = valTpy
      }

-- todo: support parse type definition
-- 'type UFixed256x18 is uint256;' or 'struct Price { uint128 price; }'
pTypeDefinition :: Parser SType
pTypeDefinition =
  STypeAlias <$> pTypeAlias <|> STypeStructure <$> pTypeStruct

-- 'type UFixed256x18 is uint256;'
-- user-defined-value-type-definition
pTypeAlias :: Parser SAlias
pTypeAlias = do
  liftA2
    ( \ident tp ->
        SAlias
          { salias = ident,
            saliasOriginType = tp
          }
    )
    ( pManySpaces
        *> pOneKeyword "type"
        *> pMany1Spaces
        *> pIdentifier
        <* pMany1Spaces
        <* pOneKeyword "is"
    )
    pType

pTypeStruct :: Parser Structure
pTypeStruct =
  liftA2
    ( \ident pairs ->
        Structure
          { structName = ident,
            structFields = pairs
          }
    )
    ( pManySpaces
        *> pOneKeyword "struct"
        *> pMany1Spaces
        *> pIdentifier
        <* pManySpaces
        <* pOneKeyword leftCurlyBrace
        <* pManySpaces
        <* many newline
    )
    ( manyTill
        ( liftA2
            (,)
            ( pManySpaces
                >> pType
            )
            ( pIdentifier
                <* ( pManySpaces
                       >> pOneKeyword semicolon
                       >> many newline
                       >> pManySpaces
                   )
            )
        )
        (pOneKeyword "}")
    )

-- { GoLeft, GoRight, GoStraight, SitStill }
enumContents :: Parser [Text]
enumContents =
  -- sepBy isn't enough here because we need to parse together
  between
    (pManySpaces *> char '{' <* pManySpaces)
    (pManySpaces >> char '}' <* pManySpaces)
    -- (sepBy pIdentifier (pManySpaces *> char ',' <* pManySpaces))
    -- todo: write the differences between the seperator
    (sepBy (pManySpaces *> pIdentifier <* pManySpaces) (char ','))

-- 'enum ActionChoices { GoLeft, GoRight, GoStraight, SitStill }'
pTypeEnum :: Parser STypeEnum
pTypeEnum = do
  enum_name <-
    pManySpaces
      >> pOneKeyword "enum"
      >> pMany1Spaces
      >> pIdentifier
        <* pManySpaces
  enums <- enumContents
  return $
    STypeEnum
      { ename = enum_name,
        eelems = enums
      }

isValidDesc :: BitLengthDesc -> Bool
isValidDesc (BitLength l) = 8 <= l && l <= 256 && l `mod` 8 == 0
isValidDesc (BitLengthWithDecimal l decimal) = 8 <= l && l <= 256 && l `mod` 8 == 0 && 0 <= decimal && decimal <= 80

extractBitLength :: BitLengthDesc -> Maybe Int
extractBitLength (BitLength n) = Just n
extractBitLength _ = Nothing

extractBitLengthWithDecimal :: BitLengthDesc -> Maybe (Int, Int)
extractBitLengthWithDecimal (BitLengthWithDecimal l d) = Just (l, d)
extractBitLengthWithDecimal _ = Nothing

-- pTypeWithDesc tries to consume an identifier as two parts, the letter parts and the integer parts
-- for the types such as int8, int16, ... int256
-- and we support the standard types in solidity only here, the custom one won't be treated as this
-- it consumes the alpha letters from the beginning, and tries to parse the following parts as a integer
-- if it fails, it will treat the indentifier as a whole and parses a result without number part
pTypeWithDesc :: Parser (Text, Maybe BitLengthDesc)
pTypeWithDesc =
  try pTypeWithLength
    <|> try pTypeWithLenDecimal
    <|> fmap (,Nothing) pIdentifier

-- parse the following types: ["int", "uint", "bytes"]
pTypeWithLength :: Parser (Text, Maybe BitLengthDesc)
pTypeWithLength = do
  prefix <- many1 letter
  guard $ prefix `elem` ["int", "uint", "bytes"]
  l <- optionMaybe pBitLength
  return (T.pack prefix, l)

pTypeWithLenDecimal :: Parser (Text, Maybe BitLengthDesc)
pTypeWithLenDecimal = do
  prefix <- many1 letter
  guard $ prefix `elem` ["fixed", "ufixed"]
  l <- pBitLengthDecimal
  return (T.pack prefix, Just l)

-- fixed-point format "MxN"
pBitLengthDecimal :: Parser BitLengthDesc
pBitLengthDecimal = do
  m <- pNumber
  n <- pOneKeyword "x" >> pNumber
  return $ BitLengthWithDecimal m n

pBitLength :: Parser BitLengthDesc
pBitLength = BitLength <$> pNumber

pBool :: (Text, Maybe BitLengthDesc) -> Parser SType
pBool (s, _) = do
  guard $ s == "bool"
  return STypeBool

pInt :: (Text, Maybe BitLengthDesc) -> Parser SType
pInt (s, bit) = do
  guard $ s == "int"
  guard $ maybe True isValidDesc bit
  return $ STypeInt $ fromMaybe 256 (bit >>= extractBitLength)

pUInt :: (Text, Maybe BitLengthDesc) -> Parser SType
pUInt (s, bit) = do
  guard $ s == "uint"
  guard $ maybe True isValidDesc bit
  return $ STypeUint $ fromMaybe 256 (bit >>= extractBitLength)

pFixed :: (Text, Maybe BitLengthDesc) -> Parser SType
pFixed (s, bit) = do
  guard $ s == "fixed"
  guard $ maybe True isValidDesc bit
  let l = maybe 128 fst (bit >>= extractBitLengthWithDecimal)
  let d = maybe 18 snd (bit >>= extractBitLengthWithDecimal)
  return $ STypeFixed l d

pUFixed :: (Text, Maybe BitLengthDesc) -> Parser SType
pUFixed (s, bit) = do
  guard $ s == "ufixed"
  guard $ maybe True isValidDesc bit
  let l = maybe 128 fst (bit >>= extractBitLengthWithDecimal)
  let d = maybe 18 snd (bit >>= extractBitLengthWithDecimal)
  return $ STypeUFixed l d

pAddress :: (Text, Maybe BitLengthDesc) -> Parser SType
pAddress (s, bit) = do
  guard $ s == "address"
  payable <- pManySpaces >> optionMaybe (pOneKeyword "payable")
  return $ maybe STypeAddress (const STypePayableAddress) payable

pStringType :: (Text, Maybe BitLengthDesc) -> Parser SType
pStringType (s, _) = do
  guard $ s == "string"
  return STypeString

pBytes :: (Text, Maybe BitLengthDesc) -> Parser SType
pBytes (s, bit) = do
  guard $ s == "bytes"
  guard $ maybe True (\x -> 1 <= x && x <= 8) (bit >>= extractBitLength)
  return $ STypeBytes $ fromMaybe 1 (bit >>= extractBitLength)

pCustom :: (Text, Maybe BitLengthDesc) -> Parser SType
pCustom (s, _) = do
  -- built-in type should be supported already inside their own parser
  -- if they fall into this parser, we should report an error to them because that's invalid
  ( if s `elem` ["int", "uint", "fixed", "ufixed", "bytes"]
      then fail "fail to define type with desired pattern"
      else return $ STypeCustom s
    )

pTypeSimple :: Parser SType
pTypeSimple =
  ( pTypeWithDesc >>= \desc ->
      asum $
        fmap
          try
          [ pBool desc,
            pInt desc,
            pUInt desc,
            pFixed desc,
            pUFixed desc,
            pAddress desc,
            pStringType desc,
            pBytes desc,
            pCustom desc
          ]
  )
    <* pManySpaces
