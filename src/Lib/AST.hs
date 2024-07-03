module Lib.AST where

import Control.Applicative (Alternative (empty), (<|>))
import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.RWS (MonadReader (ask))
import Control.Monad.State
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import Data.Char (isSpace)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Debug.Trace (trace)
import GHC.Base (Applicative (..))
import GHC.Generics (URec (UInt))
import Lib.Parser
  ( Parser,
    SemVer,
    pIdentifier,
    pMany,
    pMany1,
    pMany1Stop,
    pManySpaces,
    pOne,
    pOneKeyword,
    pOpt,
    pReadline,
    pSemVer,
    pSpace,
    pUntil,
    runParser,
  )
import Text.Read (Lexeme (String))

-- // SPDX-License-Identifier: MIT
type SPDXComment = String

-- // compiler version must be greater than or equal to 0.8.24 and less than 0.9.0
type Comment = String

-- pragma solidity ^0.8.24;
type Pragma = SemVer

data Type
  = UInt256Type
  | StringType
  | TODO
  deriving (Show, Eq)

data Function = Function
  { fmodifiers :: [String],
    fname :: String,
    fargs :: [(Type, String)],
    fReturnTyp :: Maybe Type
  }
  deriving (Show, Eq)

data Modifier = Public | Private deriving (Show, Eq)

parseModifer :: String -> Modifier
parseModifer str =
  case str of
    "public" -> Public
    "private" -> Private
    -- todo: check the default visibility of the variable
    _ -> Public -- apply the default value to it

data Variable = Variable
  { vmodifier :: Modifier,
    vtyp :: Type,
    vname :: String,
    vcomment :: Maybe Comment -- attached comment
  }
  deriving (Show, Eq)

data Contract = Contract
  { cname :: String,
    cfunctions :: [Function],
    cvariables :: [Variable]
  }
  deriving (Show, Eq)

keywordContract = "contract"

keywordPragma = "pragma"

keywordSolidity = "solidity"

keywordCommentPrefix = "//"

semicolon = ";"

leftCurlyBrace = "{"

rightCurlyBrace = "}"

leftParenthesis = "("

rightParenthesis = ")"

leftSquareBracket = "["

rightSquareBracket = "]"

data AST
  = ASTSPDXComment SPDXComment
  | ASTComment Comment
  | ASTPragma Pragma
  | ASTType Type
  | ASTFunction Function
  | ASTModifier Modifier
  | ASTVariable Variable
  | ASTContract Contract
  | Struct
      { name :: String
      }
  deriving (Show, Eq)

-- // SPDX-License-Identifier: MIT
pSPDXComment :: Parser String
pSPDXComment = do
  _ <- pOneKeyword "// SPDX-License-Identifier:"
  _ <- pManySpaces
  content <- pReadline
  let (license, _) = break isSpace content
  return license

-- // helloworld
pComment :: Parser Comment
pComment = do
  _ <- pOneKeyword "//"
  pReadline

-- pragma solidity ^0.8.24;
pPragma :: Parser Pragma
pPragma = do
  _ <- pManySpaces
  _ <- pOneKeyword keywordPragma
  _ <- pManySpaces
  _ <- pOneKeyword keywordSolidity
  _ <- pManySpaces
  version <- pSemVer
  _ <- pOneKeyword ";"
  return version

-- contract Counter {
--     uint256 public count;

--     // Function to get the current count
--     function get() public view returns (uint256) {
--         return count;
--     }
-- }
pContract :: Parser Contract
pContract = do
  _ <- pManySpaces
  _ <- pOneKeyword keywordContract
  _ <- pManySpaces
  name <- pIdentifier
  _ <- pManySpaces
  _ <- pOneKeyword leftCurlyBrace
  _ <- pManySpaces
  fields <-
    pMany
      ( fmap CtFunction pFunction
          <|> fmap CtVariable pVariable
          <|> fmap CtComment pComment
      )

  _ <- pOne rightCurlyBrace id
  let fns = mapMaybe getCtFunction fields
  let vars = mapMaybe getCtVariable fields
  return
    Contract
      { cname = name,
        cfunctions = fns,
        cvariables = vars
      }

data ContractField
  = CtFunction Function
  | CtVariable Variable
  | CtComment Comment
  | CtEmptyLine
  deriving (Eq)

getCtFunction :: ContractField -> Maybe Function
getCtFunction (CtFunction f) = Just f
getCtFunction _ = Nothing

getCtVariable :: ContractField -> Maybe Variable
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

pType :: Parser Type
pType = do
  ident <- pIdentifier
  _ <- pManySpaces
  case ident of
    "uint256" -> return UInt256Type
    "string" -> return StringType
    _ -> return TODO

pFunctionHelper :: Parser (Type, String)
pFunctionHelper = do
  _ <- pManySpaces
  _ <- pOne "," id
  _ <- pManySpaces
  tpy <- pType
  ident <- pIdentifier
  return (tpy, ident)

-- parse the content of 'bytes32 newName, bytes32 oldName' in the function below
-- function changeName(bytes32 newName, bytes32 oldName) public {
pFunctionArgs :: Parser [(Type, String)]
pFunctionArgs = ExceptT $ state $ \s -> do
  let (result, s') = runParser (liftA2 (,) pType pIdentifier) s
      re = fmap (: []) result
  case re of
    Left msg -> (Left "no args in function argument list", s)
    Right firstArg -> do
      let (argsResult, s'') = runParser (pMany pFunctionHelper) s'
      case argsResult of
        Left msg -> (Left msg, s'')
        Right args -> (Right (firstArg ++ args), s'')

pFunctionTyp :: Parser Type
pFunctionTyp = do
  _ <- pManySpaces
  _ <- pOne "(" id
  _ <- pManySpaces
  tp <- pType
  _ <- pManySpaces
  _ <- pOne ")" id
  return tp

keywordFunction = "function"

keywordReturns = "returns"

-- consume 'returns (uint256)' function return part
pReturnsClosure :: Parser Type
pReturnsClosure = do
  _ <- pManySpaces
  _ <- pOne keywordReturns id
  tp <- pFunctionTyp
  _ <- pManySpaces
  return tp

-- parse the '(name: uint)' as so on. it will consume the following spaces
pFunctionArgsQuoted :: Parser [(Type, String)]
pFunctionArgsQuoted = do
  _ <- pManySpaces
  _ <- pOne leftParenthesis id
  _ <- pManySpaces
  result <- pOpt pFunctionArgs
  let args = fromMaybe [] result
  _ <- pManySpaces
  _ <- pOne rightParenthesis id
  _ <- pManySpaces
  return args

pFunctionModifiers :: Parser [String]
pFunctionModifiers = do
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
  -- todo: think about a better way, for example refine pMany1
  return $ filter (/= "") modifiers

-- todo: whether it's called variable?
-- 'uint256 public count;' under the contract scope
pVariable :: Parser Variable
pVariable = do
  _ <- pManySpaces
  tp <- pType
  _ <- pManySpaces
  -- todo: check whether the modifer is optional
  -- todo: check whether the modifier could be many
  modifiers <- pModifier
  _ <- pManySpaces
  name <- pIdentifier
  _ <- pManySpaces
  _ <- pOne ";" id
  _ <- pManySpaces
  comment <- pOpt pComment
  return
    ( Variable
        { vmodifier = parseModifer modifiers,
          vtyp = tp,
          vname = name,
          vcomment = comment
        }
    )

pFunction :: Parser Function
pFunction = do
  _ <- pManySpaces
  _ <- pOneKeyword keywordFunction
  _ <- pManySpaces
  name <- pIdentifier
  args <- pFunctionArgsQuoted
  modifiers <- pFunctionModifiers
  optReturns <- pOpt pReturnsClosure
  _ <- pManySpaces
  _ <- pOne leftCurlyBrace id
  -- todo: parse the function body
  _ <- pUntil (== head rightCurlyBrace)
  _ <- pOne rightCurlyBrace id
  return
    ( Function
        { fname = name,
          fargs = args,
          -- fReturnTyp = Nothing,
          fReturnTyp = optReturns,
          fmodifiers = modifiers
        }
    )

-- pVariable :: Parser Variable
