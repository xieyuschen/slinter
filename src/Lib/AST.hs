module Lib.AST where

import Control.Applicative (Alternative (empty), (<|>))
import Control.Arrow (Arrow (first))
import Control.Monad.Except (ExceptT (ExceptT), MonadError (throwError), guard)
import Control.Monad.State (MonadState (..))
import Control.Monad.Trans.Except (ExceptT (ExceptT))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import Data.Bits (Bits (bit))
import Data.Char (isAlpha, isAlphaNum, isDigit, isNumber, isSpace)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, mapMaybe)
import Data.Text.Lazy.Builder.Int (decimal)
import Debug.Trace (trace)
import GHC.Base (Applicative (..), Type)
import GHC.Generics (URec (UInt))
import Lib.Parser
import Text.Read (Lexeme (String), readMaybe)

-- // SPDX-License-Identifier: MIT
type SPDXComment = String

-- // compiler version must be greater than or equal to 0.8.24 and less than 0.9.0
type Comment = String

-- pragma solidity ^0.8.24;
type Pragma = SemVer

data SType -- solidity type
  = STypeString
  | STypeBool
  | STypeInt Int -- default is 256
  | STypeUint Int -- default is 256
  | STypeFixed Int Int
  | STypeUFixed Int Int
  | -- todo: what is the address payable beside address?
    STypeAddress
  | STypePayableAddress
  | STypeBytes Int
  | STypeMapping SType SType
  | STypeArrayN SType Int
  | STypeArray SType
  | STypeCustom String
  | CustomTODO
  deriving (Show, Eq)

data Function = Function
  { fmodifiers :: [String],
    fVisiblitySpecifier :: VisibilitySpecifier,
    fname :: String,
    fargs :: [(SType, String)],
    fReturnTyp :: Maybe SType
  }
  deriving (Show, Eq)

data VisibilitySpecifier
  = VsPublic -- could be accessed from within/derived contract and external
  | VsPrivate -- only the contract it's defined in
  | VsInternal -- within contract and from derived contracts
  | VsExternal -- can only be called outside the contract
  deriving (Show, Eq)

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

data StateVariable = StateVariable
  { svVisibleSpecifier :: VisibilitySpecifier,
    svType :: SType,
    svName :: String,
    svComment :: Maybe Comment -- attached comment
  }
  deriving (Show, Eq)

data Contract = Contract
  { cname :: String,
    cfunctions :: [Function],
    cvariables :: [StateVariable]
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
  | ASTType SType
  | ASTFunction Function
  | ASTModifier VisibilitySpecifier
  | ASTVariable StateVariable
  | ASTContract Contract
  | Struct
      { name :: String
      }
  deriving (Show, Eq)

-- // SPDX-License-Identifier: MIT
pSPDXComment :: Parser String
pSPDXComment = do
  _ <- pOneKeyword "// SPDX-License-Identifier:" >> pManySpaces
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
  _ <-
    pManySpaces
      >> pOneKeyword keywordPragma
      >> pManySpaces
      >> pOneKeyword keywordSolidity
      >> pManySpaces
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
  _ <-
    pManySpaces
      >> pOneKeyword keywordContract
      >> pManySpaces
  name <- pIdentifier
  _ <-
    pManySpaces
      >> pOneKeyword leftCurlyBrace
      >> pManySpaces
  fields <-
    pMany
      ( fmap CtFunction pFunction
          <|> fmap CtVariable pStateVariable
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
  | CtVariable StateVariable
  | CtComment Comment
  | CtEmptyLine
  deriving (Eq)

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

data BitLengthDesc
  = BitLength Int
  | BitLengthWithDecimal Int Int -- the first is the bit length and the second is the decimal length
  deriving (Show, Eq)

isValidDesc :: BitLengthDesc -> Bool
isValidDesc (BitLength l) = 8 <= l && l <= 256 && l `mod` 8 == 0
isValidDesc (BitLengthWithDecimal l decimal) = 8 <= l && l <= 256 && l `mod` 8 == 0 && 0 <= decimal && decimal <= 80

extractBitLength :: BitLengthDesc -> Maybe Int
extractBitLength (BitLength n) = Just n
extractBitLength _ = Nothing

extractBitLengthWithDecimal :: BitLengthDesc -> Maybe (Int, Int)
extractBitLengthWithDecimal (BitLengthWithDecimal l d) = Just (l, d)
extractBitLengthWithDecimal _ = Nothing

-- pTypeWithBitLength tries to consume an identifier as two parts, the letter parts and the integer parts
-- for the types such as int8, int16, ... int256
-- and we support the standard types in solidity only here, the custom one won't be treated as this
-- it consumes the alpha letters from the beginning, and tries to parse the following parts as a integer
-- if it fails, it will treat the indentifier as a whole and parses a result without number part
-- todo: support fixed and ufixed, in the format of fixedMxN, where M is a multiple of 8 and range from 8 to 256 inclusive
-- the N range from 0 to 80 inclusive
pTypeWithBitLength :: Parser (String, Maybe BitLengthDesc)
pTypeWithBitLength = ExceptT $ state $ \s ->
  let (prefix, s') = span isAlpha s
      cal = foldr (liftA2 (||)) (const False) [isNumber, isAlpha, isUnderscore]
   in if null prefix
        then first Left ("Failed to parse identifier", s')
        else
          let (ident, rest) = span cal s
              (leftIdent, s'') = span cal s'
           in if prefix `notElem` ["int", "uint", "fixed", "ufixed", "bytes"]
                then do
                  -- the type is not a primitive type with bit length, we parse it as a pure type string
                  first Right ((ident, Nothing), rest)
                else
                  -- the bit length case
                  if all isNumber leftIdent && prefix `notElem` ["fixed", "ufixed"]
                    then
                      -- all leftIdent is number
                      first Right ((prefix, BitLength <$> readMaybe leftIdent), s'')
                    else
                      -- Check for fixed-point format "MxN"
                      let (mPart, nPart) = break (== 'x') leftIdent
                       in if not (null nPart) && all isNumber mPart && all isNumber (tail nPart)
                            then
                              -- ensure the mPart and nPart are numbers
                              let mValue = readMaybe mPart :: Maybe Int
                                  nValue = readMaybe (tail nPart) :: Maybe Int
                               in if isJust mValue && isJust nValue
                                    then (Right (prefix, Just $ BitLengthWithDecimal (read mPart) (read $ tail nPart)), s'')
                                    else (Right (ident, Nothing), rest)
                            else
                              -- if the suffix is not a pure number or valid fixed-point format,
                              -- we will restore and consume it as an identifier
                              (Right (ident, Nothing), rest)

pSimpleType :: Parser SType
pSimpleType = do
  (tpy, bit) <- pTypeWithBitLength
  _ <- pManySpaces
  case tpy of
    "bool" -> return STypeBool
    "int" -> do
      guard $ maybe True isValidDesc bit
      return $ STypeInt $ fromMaybe 256 (bit >>= extractBitLength)
    "uint" -> do
      guard $ maybe True isValidDesc bit
      return $ STypeUint $ fromMaybe 256 (bit >>= extractBitLength)
    "fixed" -> do
      guard $ maybe True isValidDesc bit
      let l = maybe 128 fst (bit >>= extractBitLengthWithDecimal)
      let d = maybe 18 snd (bit >>= extractBitLengthWithDecimal)
      return $ STypeFixed l d
    "ufixed" -> do
      guard $ maybe True isValidDesc bit
      let l = maybe 128 fst (bit >>= extractBitLengthWithDecimal)
      let d = maybe 18 snd (bit >>= extractBitLengthWithDecimal)
      return $ STypeUFixed l d
    "address" -> do
      _ <- pManySpaces
      payable <- pOpt $ pOne "payable" id
      case payable of
        Nothing -> return STypeAddress
        Just _ -> return STypePayableAddress
    "string" -> return STypeString
    "bytes" -> do
      guard $ maybe True (\x -> 1 <= x && x <= 8) (bit >>= extractBitLength)
      return $ STypeBytes $ fromMaybe 1 (bit >>= extractBitLength)
    _ -> return $ STypeCustom tpy

-- int / uint: Signed and unsigned integers of various sizes.
-- Keywords uint8 to uint256 in steps of 8 (unsigned of 8 up to 256 bits) and int8 to int256.
-- uint and int are aliases for uint256 and int256, respectively.
-- todo: analyzes the types with more details, such as int128 as STypeInt 128, and so on...
pType :: Parser SType
pType = do
  _ <- pManySpaces
  s <- get
  let (result, s') = runParser pTypeMapping s
  case result of
    Right m -> do
      put s'
      return m
    _ -> do
      put s -- retore the stack
      pSimpleType

pFunctionHelper :: Parser (SType, String)
pFunctionHelper = do
  _ <-
    pManySpaces
      >> pOne "," id
      >> pManySpaces
  tpy <- pType
  ident <- pIdentifier
  return (tpy, ident)

-- parse the content of 'bytes32 newName, bytes32 oldName' in the function below
-- function changeName(bytes32 newName, bytes32 oldName) public {
pFunctionArgs :: Parser [(SType, String)]
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

pFunctionTyp :: Parser SType
pFunctionTyp = do
  _ <-
    pManySpaces
      >> pOne "(" id
      >> pManySpaces
  tp <- pType
  _ <-
    pManySpaces
      >> pOne ")" id
  return tp

keywordFunction = "function"

keywordReturns = "returns"

-- consume 'returns (uint256)' function return part
pReturnsClosure :: Parser SType
pReturnsClosure = do
  _ <-
    pManySpaces
      >> pOne keywordReturns id
  tp <- pFunctionTyp
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

-- 'uint256 public count;' under the contract scope
pStateVariable :: Parser StateVariable
pStateVariable = do
  tp <- pManySpaces >> pType
  -- state variable only has visiblity specifier, we don't need to parse the modifiers
  specifier <- pManySpaces >> pVisibilitySpecifier
  name <- pManySpaces >> pIdentifier
  _ <-
    pManySpaces
      >> pOne ";" id
  comment <- pManySpaces >> pOpt pComment
  return
    ( StateVariable
        { svVisibleSpecifier = specifier,
          svType = tp,
          svName = name,
          svComment = comment
        }
    )

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
  optReturns <- pOpt pReturnsClosure
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

-- todo: support me
-- 'mapping(KeyType KeyName? => ValueType ValueName?)'
-- The KeyType can be any built-in value type, bytes, string, or any contract or enum type. O
-- ther user-defined or complex types, such as mappings, structs or array types are not allowed.
-- ValueType can be any type, including mappings, arrays and structs.
-- KeyName and ValueName are optional (so mapping(KeyType => ValueType) works as well)
-- and can be any valid identifier that is not a type.
pTypeMapping :: Parser SType
pTypeMapping = do
  _ <- pManySpaces >> pOne "mapping(" id >> pManySpaces
  keyTyp <- pType
  -- todo: check the key type should be built-in value type, bytes, string, or any contract or enum type
  _ <- pOpt pIdentifier -- parse the name, todo: check whether need to store the name
  _ <- pManySpaces >> pOne "=>" id >> pManySpaces
  valTpy <- pType
  _ <- pOpt pIdentifier >> pOne ")" id >> pManySpaces
  return (STypeMapping keyTyp valTpy)

-- todo: support parse type definition
-- 'type UFixed256x18 is uint256;'
pTypeDefinition :: Parser String
pTypeDefinition = pIdentifier

-- todo: support parse enum scope:
-- 'enum ActionChoices { GoLeft, GoRight, GoStraight, SitStill }'
pEnum :: Parser String
pEnum = pIdentifier
