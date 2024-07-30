{-# LANGUAGE OverloadedStrings #-}

module Lib.AST.Function where

import Control.Applicative
  ( Alternative (),
    Applicative (liftA2),
    liftA3,
    optional,
  )
import Control.Monad (guard)
import Control.Monad.Except
  ( ExceptT (ExceptT),
    MonadError (throwError),
    void,
  )
import Control.Monad.State (MonadState (..))
import Data.Foldable
import Data.Functor (($>))
import Data.Maybe (catMaybes, fromMaybe, isNothing, listToMaybe, mapMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace
import GHC.Base ()
import Lib.AST.Expr (pExpression, pLocationModifier)
import Lib.AST.Model
  ( ContractField (CtFunction, CtVariable),
    DataLocation (Storage),
    FnDeclArg (..),
    FnDecorator (..),
    FnName (..),
    FnStateMutability (..),
    FnVisibility (..),
    Function (..),
    SType,
    StateVariable,
    extractFnDecS,
    extractFnDecV,
    keywordFunction,
    keywordReturns,
    leftCurlyBrace,
    leftParenthesis,
    rightCurlyBrace,
    rightParenthesis,
  )
import Lib.AST.Type (pType)
import Lib.AST.Util (toFnVisibility)
import Lib.Parser
import Text.Parsec

getCtFunction :: ContractField -> Maybe Function
getCtFunction (CtFunction f) = Just f
getCtFunction _ = Nothing

getCtVariable :: ContractField -> Maybe StateVariable
getCtVariable (CtVariable v) = Just v
getCtVariable _ = Nothing

pModifier :: Parser String
pModifier = do
  modifier <- pManySpaces >> pIdentifier
  case modifier of
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
        (optionMaybe $ pManySpaces >> pLocationModifier)
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
      *> pOneKeyword keywordReturns
      *> pMany1Spaces
      *> pFunctionReturnTypeWithQuote
      <* pManySpaces

-- parse the '(name: uint)' as so on. it will consume the following spaces
pFunctionArgsInParentheses :: Parser [FnDeclArg]
pFunctionArgsInParentheses = do
  fmap (fromMaybe []) $
    pManySpaces
      >> pOneKeyword leftParenthesis
      >> pManySpaces
      >> optionMaybe pFunctionArgs
        <* ( pManySpaces
               >> pOneKeyword rightParenthesis
               >> pManySpaces
           )

-- parse the visibility of function definition
pFnDeclVisibility :: Parser FnVisibility
pFnDeclVisibility =
  do
    try (pOneKeyword "public") $> FnPublic
    <|> try (pOneKeyword "private") $> FnPrivate
    <|> try (pOneKeyword "internal") $> FnInternal
    <|> try (pOneKeyword "external") $> FnExternal

-- parse the state-mutability of function definition
pFnDeclStateMutability :: Parser FnStateMutability
pFnDeclStateMutability =
  do
    try (pOneKeyword "pure") $> FnStatePure
    <|> try (pOneKeyword "view") $> FnStateView
    <|> try (pOneKeyword "payable") $> FnStatePayable

-- todo: implement me
pFnDeclModifierInvocation :: Parser Text
pFnDeclModifierInvocation = return ""

-- whether the function is decorated by the 'virtual' keyword
pFnDeclVirtual :: Parser FnDecorator
pFnDeclVirtual = try (pOneKeyword "virtual") $> FnDecVirtual <|> return FnDecSkip

-- todo: implement me
pFnDeclOverrideSpecifier :: Parser Text
pFnDeclOverrideSpecifier = return ""

-- parse all decorators(modifiers and visibility specifiers) separated by spaces into a list of string
-- todo: this parser has a flaw that cannot parse the 'public  view' correctly due to the pMany1Spaces,
--  however it's acceptable because this is not a valid syntax in solidity
pFunctionDecorators :: Parser [FnDecorator]
pFunctionDecorators = do
  decorators <-
    pManySpaces
      *> manyTill
        ( ( FnDecV <$> try pFnDeclVisibility
              <|> (FnDecS <$> try pFnDeclStateMutability)
              <|> try pFnDeclVirtual
          )
            <* pMany1Spaces
        )
        ( lookAhead $
            try (pOneKeyword "returns")
              <|> try (pOneKeyword "{")
              <|> eof $> ""
        )

  -- we need to filter the FnDecSkip string,
  return $ filter (/= FnDecSkip) decorators

pFunctionName :: Parser FnName
pFunctionName =
  do
    pOneKeyword "fallback" $> FnFallback
    <|> pOneKeyword "receive" $> FnReceive
    <|> FnNormal <$> pIdentifier

pFunction :: Parser Function
pFunction = do
  name <-
    pManySpaces
      >> pOneKeyword keywordFunction
      >> pMany1Spaces
      >> pFunctionName
  args <- pManySpaces >> pFunctionArgsInParentheses

  -- todo: support custom modifiers as well
  decorators <- pFunctionDecorators
  optReturns <- optionMaybe pReturnsClause

  let visibility = extractFnDecV decorators
  guard $ length visibility <= 1

  let states = extractFnDecS decorators
  guard $ length states <= 1

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
          fnIsVirtual = FnDecVirtual `elem` decorators,
          fnVisibility = fromMaybe FnInternal $ listToMaybe visibility,
          fnState = fromMaybe FnStateDefault $ listToMaybe states,
          fReturnTyp = optReturns
        }
    )
