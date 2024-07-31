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
import Lib.AST.Expr (pExpression, pFnCallArgumentList)
import Lib.AST.Model
  ( ContractField (CtFunction, CtVariable),
    DataLocation (Storage),
    FnDeclArg (..),
    FnDecorator (..),
    FnModifierInvocation (..),
    FnName (..),
    FnStateMutability (..),
    FnVisibility (..),
    Function (..),
    OverrideSpecifier,
    SType,
    StateVariable,
    extractFnDecMI,
    extractFnDecOs,
    extractFnDecS,
    extractFnDecV,
    keywordFunction,
    keywordReturns,
    leftCurlyBrace,
    leftParenthesis,
    rightCurlyBrace,
    rightParenthesis,
    semicolon,
  )
import Lib.AST.Stat (pState)
import Lib.AST.Type (pType)
import Lib.AST.Util
import Lib.Parser
import Text.Parsec

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

  -- guards the decorators satisify the function declaration specification
  let visibility = extractFnDecV decorators
      states = extractFnDecS decorators
      mis = extractFnDecMI decorators
      ospecifier = extractFnDecOs decorators
  guard $ length visibility <= 1
  guard $ length states <= 1
  guard $ length states <= 1

  optReturns <- optionMaybe pReturnsClause
  fnBody <-
    pManySpaces
      *> pOneKeyword semicolon
      $> Nothing
      <|> Just
        <$> between
          (pOneKeyword leftCurlyBrace >> pManySpaces)
          (pOneKeyword rightCurlyBrace)
          (many $ pState <* pManySpaces)

  -- why 'many anyChar' doesn't work?
  return
    ( Function
        { fname = name,
          fargs = args,
          fnIsVirtual = FnDecVirtual `elem` decorators,
          fnVisibility = fromMaybe FnInternal $ listToMaybe visibility,
          fnState = fromMaybe FnStateDefault $ listToMaybe states,
          fnModifierInvocations = mis,
          fnFnOverrideSpecifier = listToMaybe ospecifier,
          fnReturnTyp = optReturns,
          fnBody = fnBody
        }
    )

getCtFunction :: ContractField -> Maybe Function
getCtFunction (CtFunction f) = Just f
getCtFunction _ = Nothing

getCtVariable :: ContractField -> Maybe StateVariable
getCtVariable (CtVariable v) = Just v
getCtVariable _ = Nothing

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

-- parse the state-mutability of function definition
pFnDeclStateMutability :: Parser FnStateMutability
pFnDeclStateMutability =
  do
    try (pOneKeyword "pure") $> FnStatePure
    <|> try (pOneKeyword "view") $> FnStateView
    <|> try (pOneKeyword "payable") $> FnStatePayable

pFnDeclModifierInvocation :: Parser FnModifierInvocation
pFnDeclModifierInvocation = do
  path <- pIdentifierPath
  args <- optionMaybe pFnCallArgumentList
  return
    FnModifierInvocation
      { fnModifierInvocationArgs = args,
        fnModifierInvocationPath = path
      }

-- parse all decorators(modifiers and visibility specifiers) separated by spaces into a list of string
-- todo: this parser has a flaw that cannot parse the 'public  view' correctly due to the pMany1Spaces,
--  however it's acceptable because this is not a valid syntax in solidity
pFunctionDecorators :: Parser [FnDecorator]
pFunctionDecorators = do
  pManySpaces
    *> manyTill
      ( ( FnDecV <$> try pFnDeclVisibility
            <|> pFnDeclVirtual
            <|> (FnDecS <$> try pFnDeclStateMutability)
            <|> (FnDecOs <$> try pOverrideSpecifier)
            -- modifier invocation should be put at last,
            -- otherwise it will process the 'override' and 'virtual' as a modifier invocation,
            -- which is definitely wrong
            <|> (FnDecMI <$> try pFnDeclModifierInvocation)
        )
          <* pMany1Spaces
      )
      ( lookAhead $
          try (pOneKeyword "returns")
            <|> try (pOneKeyword "{")
            <|> eof $> ""
      )

pFunctionName :: Parser FnName
pFunctionName =
  do
    pOneKeyword "fallback" $> FnFallback
    <|> pOneKeyword "receive" $> FnReceive
    <|> FnNormal <$> pIdentifier
