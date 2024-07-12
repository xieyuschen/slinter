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
import Lib.AST.Expr (pExpression)
import Lib.AST.Model
  ( ContractField (CtFunction, CtVariable),
    ExprFnCall (..),
    FnCallArgs (..),
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

-- todo: refine me to avoid case of usage
-- parse the content of 'bytes32 newName, bytes32 oldName' in the function below
-- function changeName(bytes32 newName, bytes32 oldName) public {
pFunctionArgs :: Parser [(SType, Maybe String)]
pFunctionArgs =
  liftA2
    (:)
    (liftA2 (,) pType $ optional pIdentifier)
    (many pFunctionDotAndArg)

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
pFunctionArgsQuoted :: Parser [(SType, Maybe String)]
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

pFunctionDotAndArg :: Parser (SType, Maybe String)
pFunctionDotAndArg =
  liftA2
    (,)
    ( pManySpaces
        >> pOneKeyword ","
        >> pManySpaces
        >> pType
    )
    (optional pIdentifier)

pVisibilitySpecifier :: Parser VisibilitySpecifier
pVisibilitySpecifier = do
  ident <- pManySpaces >> pIdentifier
  case toVisibilitySpecifier ident of
    Just specifier -> return specifier
    Nothing -> error "not a valid visible specifier"

toVisibilitySpecifier :: String -> Maybe VisibilitySpecifier
toVisibilitySpecifier str =
  case str of
    "public" -> return VsPublic
    "private" -> return VsPrivate
    "internal" -> return VsInternal
    "external" -> return VsExternal
    _ -> Nothing

pFuncCall :: Parser ExprFnCall
pFuncCall = do
  (ct, fName) <-
    pManySpaces
      >> liftA2
        (,)
        -- use pTry here to make sure the identifier and keyword are consumed in a batch
        (optional $ pTry $ pIdentifier <* pOneKeyword ".")
        pIdentifier
  args <- pTry pFuncCallArgsNamedParameters <|> pTry pFuncCallArgsList
  return
    ExprFnCall
      { fnContractName = ct,
        fnName = fName,
        fnArguments = args
      }

pFuncCallNamedParameterKeyValue :: Parser (String, SExpr)
pFuncCallNamedParameterKeyValue =
  liftA2
    (,)
    (pManySpaces >> pIdentifier <* (pManySpaces >> pOneKeyword ":"))
    pExpression

pFuncCallArgsNamedParameters :: Parser FnCallArgs
pFuncCallArgsNamedParameters = do
  arg1 <-
    pManySpaces
      >> pOneKeyword leftParenthesis
      >> pManySpaces
      >> pOneKeyword leftCurlyBrace
      >> optional pFuncCallNamedParameterKeyValue
  args <-
    many
      ( pManySpaces
          >> pOneKeyword "," -- todo: this matching should be unified in a function in parser
          >> pManySpaces
          >> pTry pFuncCallNamedParameterKeyValue
      )
  s <- get
  trace (show args) $ pure ()
  trace (s) $ pure ()
  _ <-
    pOneKeyword rightCurlyBrace
      >> pManySpaces
      >> pOneKeyword rightParenthesis
  return $ FnCallArgsNamedParameters $ maybeToList arg1 ++ args

pFuncCallArgsList :: Parser FnCallArgs
pFuncCallArgsList = do
  arg1 <-
    pManySpaces
      >> pOneKeyword leftParenthesis
      >> optional pExpression
  args <-
    many $
      pManySpaces
        >> pOneKeyword ","
        >> pManySpaces
        >> pExpression
  _ <- pOneKeyword rightParenthesis
  return $ FnCallArgsList $ maybeToList arg1 ++ args
