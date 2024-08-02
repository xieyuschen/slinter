{-# LANGUAGE OverloadedStrings #-}

module Lib.AST.Pragma where

import Control.Applicative (Applicative (liftA2))
import Control.Monad (guard, when)
import Data.Char (isSpace)
import Data.Functor (($>))
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Lib.AST.Model
  ( Comment,
    IdentifierPath,
    ImportDirective (..),
    ImportedPath,
    Operator
      ( ArithmeticAddition,
        ArithmeticDivision,
        ArithmeticModulus,
        ArithmeticMultiplication,
        BitAnd,
        BitExor,
        BitNeg,
        BitOr,
        ComparisonLess,
        ComparisonLessEqual,
        ComparisonMore,
        ComparisonMoreEqual,
        LogicalEqual,
        LogicalInequal,
        Minus
      ),
    Pragma,
    SymbolAlias (..),
    UserDefinableOperator,
    UsingAlias (..),
    UsingDirective (..),
    UsingField (..),
    UsingType (..),
    keywordPragma,
    keywordSolidity,
    leftCurlyBrace,
    rightCurlyBrace,
    semicolon,
  )
import Lib.AST.Oper (pOperator)
import Lib.AST.Type (pType)
import Lib.AST.Util
import Lib.Parser
  ( Parser,
    pIdentifier,
    pMany1Spaces,
    pManySpaces,
    pOneKeyword,
    pReadline,
    pSemVer,
    pString,
  )
import Text.Parsec

-- // SPDX-License-Identifier: MIT
pSPDXComment :: Parser Text
pSPDXComment = do
  content <- pOneKeyword "// SPDX-License-Identifier:" >> pManySpaces >> pReadline
  let (license, _) = T.break isSpace content
  return license

-- // helloworld
pComment :: Parser Comment
pComment = do
  pOneKeyword "//" >> pReadline

-- pragma solidity ^0.8.24;
pPragma :: Parser Pragma
pPragma = do
  pManySpaces
    >> pOneKeyword keywordPragma
    >> pMany1Spaces
    >> pOneKeyword keywordSolidity
    >> pMany1Spaces
    >> pSemVer
      <* pOneKeyword ";"

-- todo: improve this to support absolute/relative path
pImportPath :: Parser ImportedPath
pImportPath = pString

pImportDirective :: Parser ImportDirective
pImportDirective =
  pManySpaces
    *> pOneKeyword "import"
    *> ( try pImportedPath
           <|> try pImportSymbolAliaes
           <|> try pImportAllSymbols
       )

-- import "./MyLibrary.sol";
-- import "./MyLibrary.sol" as MyLib;
pImportedPath :: Parser ImportDirective
pImportedPath = do
  path <- pMany1Spaces *> pImportPath
  ident <-
    optionMaybe
      ( pMany1Spaces
          *> pOneKeyword "as"
          *> pMany1Spaces
          *> pIdentifier
      )
  _ <- pManySpaces >> pOneKeyword semicolon
  case ident of
    Just i -> return $ ImportPathWithAlias path i
    Nothing -> return $ ImportPath path

pImportAliases :: Parser [SymbolAlias]
pImportAliases = do
  pManySpaces
    *> between
      (pManySpaces *> pOneKeyword leftCurlyBrace <* pManySpaces)
      (pManySpaces *> pOneKeyword rightCurlyBrace <* pManySpaces)
      ( sepBy
          ( liftA2
              (\ident alias -> SymbolAlias {symbolIdentifier = ident, symbolAlias = alias})
              ( try
                  ( pManySpaces -- don't consume the spaces and trigger the sepBy
                      *> pIdentifier
                      <* pManySpaces
                  )
              )
              ( optionMaybe $
                  pOneKeyword "as"
                    *> pMany1Spaces
                    *> pIdentifier
                    <* pManySpaces
              )
          )
          (char ',')
      )

-- import {MySymbol} from "./MyLibrary.sol";
-- import {MySymbol as RenamedSymbol} from "./MyLibrary.sol";
pImportSymbolAliaes :: Parser ImportDirective
pImportSymbolAliaes =
  liftA2
    (flip ImportSymbolAliases)
    pImportAliases
    ( pManySpaces
        *> pOneKeyword "from"
        *> pMany1Spaces
        *> pImportPath
        <* pManySpaces
        <* pOneKeyword semicolon
    )

-- import * as MyLib from "./MyLibrary.sol";
pImportAllSymbols :: Parser ImportDirective
pImportAllSymbols = do
  ident <-
    pMany1Spaces -- don't allow 'import*', there must be an space between them
      *> pOneKeyword "*"
      *> pMany1Spaces
      *> pOneKeyword "as"
      *> pMany1Spaces
      *> pIdentifier
  path <-
    pMany1Spaces
      *> pOneKeyword "from"
      *> pMany1Spaces
      *> pImportPath
      <* pManySpaces
      <* pOneKeyword semicolon
  return $ ImportAllSymbols path ident

-- parse the whole using directive, such as:
-- 'using {sub as -, neg as -, add as +} for Bitmap global;'
-- 'using {clear_count} for User global;'
-- 'using {mask, odd} for *;'
pUsingDirective :: Parser UsingDirective
pUsingDirective = do
  field <-
    pManySpaces
      *> pOneKeyword "using"
      *> pMany1Spaces
      *> pUsingField
  -- because pIdentifierPath consumes all letters,
  -- we needn't to care about the letters are combined together
  utp <-
    pManySpaces
      >> pOneKeyword "for"
      >> pMany1Spaces
        *> pUsingType
        <* pManySpaces
  isGloabl <-
    -- don't worry the type and global combine together,
    -- because the type parser will consume the identifier until a space
    pOneKeyword "global" $> True
      <|> return False
  _ <- pManySpaces >> pOneKeyword semicolon

  return
    UsingDirective
      { usingDirectiveField = field,
        usingType = utp,
        usingGlobal = isGloabl
      }

-- parse '*' or types('User' here) in using directive such as:
-- 'using {mask, odd} for *;'
-- 'using {clear_count} for User';
pUsingType :: Parser UsingType
pUsingType =
  do
    pOneKeyword "*" $> UsingAll
    <|> UsingTp <$> pType

-- convert the operator to a supported operator in using directive,
-- because not all operators are valid as definable operators
toUserDefinableOperator :: Operator -> Maybe UserDefinableOperator
toUserDefinableOperator o =
  if o
    `elem` [ LogicalEqual,
             LogicalInequal,
             ArithmeticAddition,
             Minus,
             ArithmeticMultiplication,
             ArithmeticDivision,
             ArithmeticModulus,
             ComparisonLessEqual,
             ComparisonLess,
             ComparisonMoreEqual,
             ComparisonMore,
             BitAnd,
             BitOr,
             BitExor,
             BitNeg
           ]
    then Just o
    else Nothing

-- the 'a.b.c' part of using directive 'using a.b.c for Bitmap'
-- and the curly quoted parts mentioned by pUsingFieldAliases
pUsingField :: Parser UsingField
pUsingField =
  UFIdentifierPath <$> pIdentifierPath
    <|> pUsingFieldAliases

-- the '{sub as -, neg as -, add as +}' part of using directive 'using {sub as -, neg as -, add as +} for Bitmap global;'
-- the '{clear_count}' part of using directiv 'using {clear_count} for User global;'
pUsingFieldAliases :: Parser UsingField
pUsingFieldAliases =
  UFUsingAliases
    <$> between
      (pManySpaces *> pOneKeyword leftCurlyBrace <* pManySpaces)
      (pManySpaces *> pOneKeyword rightCurlyBrace <* pManySpaces)
      (sepBy (try pUsingFieldAlias) (char ','))

-- parse single unit of the filed alias such as 'a.b.c as &',
-- the 'as &' part is optional
pUsingFieldAlias :: Parser UsingAlias
pUsingFieldAlias = do
  path <- pManySpaces *> pIdentifierPath

  op <-
    optionMaybe
      ( try $
          pMany1Spaces
            *> pOneKeyword "as"
            *> pMany1Spaces
            *> pOperator
            <* pManySpaces
      )
  when (isNothing op) pManySpaces

  return $
    UsingAlias
      { uaIdentifierPath = path,
        uaUserDefinableOper = op >>= toUserDefinableOperator
      }
