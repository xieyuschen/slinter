module Lib.AST.Function where

import Control.Applicative
import Control.Monad.Except
  ( ExceptT (ExceptT),
    MonadError (throwError),
  )
import Control.Monad.State (MonadState (..))
import Data.Maybe (catMaybes, fromMaybe, isNothing, mapMaybe, maybeToList)
import Debug.Trace
import GHC.Base ()
import Lib.AST.Expr (pExpression, pLocationModifer)
import Lib.AST.Model
  ( ContractField (CtFunction, CtVariable),
    DataLocation (Storage),
    ExprFnCall (..),
    FnCallArgs (..),
    FnDeclArg (..),
    Function (..),
    SExpr,
    SType,
    StateVariable,
    VisibilitySpecifier (..),
    keywordFunction,
    keywordReturns,
    leftCurlyBrace,
    leftParenthesis,
    rightCurlyBrace,
    rightParenthesis,
  )
import Lib.AST.Type (pType)
import Lib.AST.Util
import Lib.Parser
  ( Parser,
    pIdentifier,
    pMany1Stop,
    pManySpaces,
    pOneKeyword,
    pSpace,
    pTry,
    pUntil,
    runParser,
  )

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
        (optional $ pManySpaces >> pLocationModifer)
        (optional $ pManySpaces >> pIdentifier)

pFunctionReturnTypeWithQuote :: Parser SType
pFunctionReturnTypeWithQuote =
  pManySpaces
    >> pOneKeyword "("
    >> pManySpaces
    >> pType
      <* ( pManySpaces
             >> pOneKeyword ")"
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
      >> optional pFunctionArgs
        <* ( pManySpaces
               >> pOneKeyword rightParenthesis
               >> pManySpaces
           )

-- parse all decorators(modifers and visibility specifiers) seperated by whitespaces into a list of string
pFunctionDecorators :: Parser [String]
pFunctionDecorators = do
  modifiers <-
    pManySpaces
      *> pMany1Stop
        ( pIdentifier
            <|> fmap (const "") pSpace
        )
        "returns"
      <* pManySpaces
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
    _ -> throwError "visibility specifier should contain only one for each function"
  let modifiers = filter (isNothing . toVisibilitySpecifier) decorators
  optReturns <-
    optional pReturnsClause
      <* ( pManySpaces
             >> pOneKeyword leftCurlyBrace
             -- todo: parse the function body
             >> pUntil (== head rightCurlyBrace)
             >> pOneKeyword rightCurlyBrace
         )
  return
    ( Function
        { fname = name,
          fargs = args,
          fVisiblitySpecifier = specifier,
          fReturnTyp = optReturns,
          fmodifiers = modifiers
        }
    )
