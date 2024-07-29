{-# LANGUAGE OverloadedStrings #-}

module Lib.AST.Function where

import Control.Applicative
  ( Alternative (),
    Applicative (liftA2),
    liftA3,
    optional,
  )
import Control.Monad.Except
  ( ExceptT (ExceptT),
    MonadError (throwError),
  )
import Control.Monad.State (MonadState (..))
import Data.Functor (($>))
import Data.Maybe (catMaybes, fromMaybe, isNothing, mapMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Base ()
import Lib.AST.Expr (pExpression, pLocationModifer)
import Lib.AST.Model
  ( ContractField (CtFunction, CtVariable),
    DataLocation (Storage),
    FnDeclArg (..),
    Function (..),
    SType,
    StateVariable,
    keywordFunction,
    keywordReturns,
    leftCurlyBrace,
    leftParenthesis,
    rightCurlyBrace,
    rightParenthesis,
  )
import Lib.AST.Type (pType)
import Lib.AST.Util (toVisibilitySpecifier)
import Lib.Parser (Parser, pIdentifier, pManySpaces, pOneKeyword)
import Text.Parsec

getCtFunction :: ContractField -> Maybe Function
getCtFunction (CtFunction f) = Just f
getCtFunction _ = Nothing

getCtVariable :: ContractField -> Maybe StateVariable
getCtVariable (CtVariable v) = Just v
getCtVariable _ = Nothing

pModifier :: Parser String
pModifier = do
  modifer <- pManySpaces >> pIdentifier
  case modifer of
    "public" -> return "public"
    "view" -> return "view"
    _ -> error ""

pFunctionArgs :: Parser [FnDeclArg]
pFunctionArgs =
  liftA2
    (:)
    com
    (many $ pOneKeyword "," >> com)
  where
    com =
      liftA3
        ( \tp modifier name ->
            FnDeclArg
              { fnArgTp = tp,
                fnArgName = name,
                fnArgLocation = fromMaybe Storage modifier
              }
        )
        pType
        (optionMaybe $ pManySpaces >> pLocationModifer)
        (optionMaybe $ pManySpaces >> pIdentifier)

pFunctionReturnTypeWithQuote :: Parser SType
pFunctionReturnTypeWithQuote =
  pManySpaces
    >> pOneKeyword leftParenthesis
    >> pManySpaces
    >> pType
      <* ( pManySpaces
             >> pOneKeyword rightParenthesis
         )

-- consume 'returns (uint256)' function return part
pReturnsClause :: Parser SType
pReturnsClause =
  do
    pManySpaces
      >> pOneKeyword keywordReturns
    >> pFunctionReturnTypeWithQuote <* pManySpaces

-- parse the '(name: uint)' as so on. it will consume the following spaces
pFunctionArgsQuoted :: Parser [FnDeclArg]
pFunctionArgsQuoted = do
  fmap (fromMaybe []) $
    pManySpaces
      >> pOneKeyword leftParenthesis
      >> pManySpaces
      >> optionMaybe pFunctionArgs
        <* ( pManySpaces
               >> pOneKeyword rightParenthesis
               >> pManySpaces
           )

-- parse all decorators(modifers and visibility specifiers) seperated by whitespaces into a list of string
pFunctionDecorators :: Parser [Text]
pFunctionDecorators = do
  modifiers <-
    pManySpaces
      >> manyTill
        -- it's fine to use pManySpaces because the pIdentifier will always consume the non-empty string,
        -- we needn't to concern the two keywords are appened together like the keyword
        (pIdentifier <* pManySpaces)
        (lookAhead $ try (pOneKeyword "returns" <|> pOneKeyword "{" <|> (eof $> "")))

  -- we need to filter the empty string,
  -- because we omit the empty string for space
  return $ filter (/= "") modifiers

pFunction :: Parser Function
pFunction = do
  name <-
    pManySpaces
      >> pOneKeyword keywordFunction
      >> pManySpaces
      >> pIdentifier
  args <- pManySpaces >> pFunctionArgsQuoted

  -- todo: support custom modifiers as well
  decorators <- pFunctionDecorators
  let specifiers = mapMaybe toVisibilitySpecifier decorators
  specifier <- case length specifiers of
    1 -> return $ head specifiers
    _ -> fail "visibility specifier should contain only one for each function"
  let modifiers = filter (isNothing . toVisibilitySpecifier) decorators
  optReturns <- optionMaybe pReturnsClause

  -- todo: parse function body
  _ <-
    pManySpaces
      >> between
        (pOneKeyword leftCurlyBrace)
        (pOneKeyword rightCurlyBrace)
        (many $ noneOf "}")
  -- why 'many anyChar' doesn't work?

  return
    ( Function
        { fname = name,
          fargs = args,
          fVisiblitySpecifier = specifier,
          fReturnTyp = optReturns,
          fmodifiers = modifiers
        }
    )
