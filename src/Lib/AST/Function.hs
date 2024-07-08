module Lib.AST.Function where

import Control.Applicative
import Control.Monad.Except
  ( ExceptT (ExceptT),
    MonadError (throwError),
  )
import Control.Monad.State (MonadState (state))
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import GHC.Base ()
import Lib.AST.Model
  ( ContractField (CtFunction, CtVariable),
    Function (..),
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
    pMany,
    pMany1Stop,
    pManySpaces,
    pOne,
    pOneKeyword,
    pOpt,
    pSpace,
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
  _ <- pManySpaces
  modifer <- pIdentifier
  case modifer of
    "public" -> return "public"
    "view" -> return "view"
    _ -> error ""

-- parse the content of 'bytes32 newName, bytes32 oldName' in the function below
-- function changeName(bytes32 newName, bytes32 oldName) public {
pFunctionArgs :: Parser [(SType, String)]
pFunctionArgs = ExceptT $ state $ \s -> do
  let (result, s') = runParser (liftA2 (,) pType pIdentifier) s
      re = fmap (: []) result
  case re of
    Left msg -> (Left $ "no args in function argument list: " ++ msg, s)
    Right firstArg -> do
      let (argsResult, s'') = runParser (pMany pFunctionHelper) s'
      case argsResult of
        Left msg -> (Left msg, s'')
        Right args -> (Right (firstArg ++ args), s'')

pFunctionReturnTypeWithQuote :: Parser SType
pFunctionReturnTypeWithQuote = do
  _ <-
    pManySpaces
      >> pOne "(" id
      >> pManySpaces
  tp <- pType
  _ <-
    pManySpaces
      >> pOne ")" id
  return tp

-- consume 'returns (uint256)' function return part
pReturnsClause :: Parser SType
pReturnsClause = do
  _ <-
    pManySpaces
      >> pOne keywordReturns id
  tp <- pFunctionReturnTypeWithQuote
  _ <- pManySpaces
  return tp

-- parse the '(name: uint)' as so on. it will consume the following spaces
pFunctionArgsQuoted :: Parser [(SType, String)]
pFunctionArgsQuoted = do
  _ <-
    pManySpaces
      >> pOne leftParenthesis id
      >> pManySpaces
  result <- pOpt pFunctionArgs
  let args = fromMaybe [] result
  _ <-
    pManySpaces
      >> pOne rightParenthesis id
      >> pManySpaces
  return args

-- parse all decorators(modifers and visibility specifiers) seperated by whitespaces into a list of string
pFunctionDecorators :: Parser [String]
pFunctionDecorators = do
  _ <- pManySpaces
  modifiers <-
    pMany1Stop
      ( pIdentifier
          <|> fmap (const "") pSpace
      )
      "returns"
  _ <- pManySpaces
  -- we need to filter the empty string,
  -- because we omit the empty string for space
  return $ filter (/= "") modifiers

pFunction :: Parser Function
pFunction = do
  _ <-
    pManySpaces
      >> pOneKeyword keywordFunction
  name <- pManySpaces >> pIdentifier
  args <- pManySpaces >> pFunctionArgsQuoted

  -- todo: support custom modifiers as well
  decorators <- pFunctionDecorators
  let specifiers = mapMaybe toVisibilitySpecifier decorators
  specifier <- case length specifiers of
    1 -> return $ head specifiers
    _ -> throwError "visibility specifier should contain only one for each function"
  let modifiers = filter (isNothing . toVisibilitySpecifier) decorators
  optReturns <- pOpt pReturnsClause
  _ <-
    pManySpaces
      >> pOne leftCurlyBrace id
      -- todo: parse the function body
      >> pUntil (== head rightCurlyBrace)
      >> pOne rightCurlyBrace id
  return
    ( Function
        { fname = name,
          fargs = args,
          fVisiblitySpecifier = specifier,
          fReturnTyp = optReturns,
          fmodifiers = modifiers
        }
    )

pFunctionHelper :: Parser (SType, String)
pFunctionHelper = do
  _ <-
    pManySpaces
      >> pOne "," id
      >> pManySpaces
  tpy <- pType
  ident <- pIdentifier
  return (tpy, ident)

pVisibilitySpecifier :: Parser VisibilitySpecifier
pVisibilitySpecifier = do
  _ <- pManySpaces
  ident <- pIdentifier
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
    --
    _ -> Nothing
