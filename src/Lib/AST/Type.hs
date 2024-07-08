module Lib.AST.Type where

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
import Lib.AST.Model
import Lib.Parser
import Text.Read (Lexeme (String), readMaybe)

-- int / uint: Signed and unsigned integers of various sizes.
-- Keywords uint8 to uint256 in steps of 8 (unsigned of 8 up to 256 bits) and int8 to int256.
-- uint and int are aliases for uint256 and int256, respectively.
pType :: Parser SType
pType = do
  _ <- pManySpaces
  s <- get
  let (result, s') = runParser pTypeMapping s
  case result of
    Right m -> do
      put s'
      return $ STypeMapping m
    _ -> do
      put s -- retore the stack
      pTypeSimple

-- todo: support me
-- 'mapping(KeyType KeyName? => ValueType ValueName?)'
-- The KeyType can be any built-in value type, bytes, string, or any contract or enum type. O
-- ther user-defined or complex types, such as mappings, structs or array types are not allowed.
-- ValueType can be any type, including mappings, arrays and structs.
-- KeyName and ValueName are optional (so mapping(KeyType => ValueType) works as well)
-- and can be any valid identifier that is not a type.
pTypeMapping :: Parser Mapping
pTypeMapping = do
  _ <- pManySpaces >> pOne "mapping(" id >> pManySpaces
  keyTyp <- pType
  -- todo: check the key type should be built-in value type, bytes, string, or any contract or enum type
  _ <- pOpt pIdentifier -- parse the name, todo: check whether need to store the name
  _ <- pManySpaces >> pOne "=>" id >> pManySpaces
  valTpy <- pType
  _ <- pOpt pIdentifier >> pOne ")" id >> pManySpaces
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
pTypeAlias :: Parser SAlias
pTypeAlias = do
  _ <-
    pManySpaces
      >> pOne "type" id
      >> pManySpaces
  ident <- pIdentifier
  _ <-
    pManySpaces
      >> pOne "is" id
  tp <- pType
  return $
    SAlias
      { salias = ident,
        saliasOriginType = tp
      }

pTypeStruct :: Parser Structure
pTypeStruct = do
  _ <-
    pManySpaces
      >> pOne "struct" id
      >> pManySpaces
  ident <- pIdentifier
  _ <-
    pManySpaces
      >> pOne "{" id
      >> pManySpaces
  pairs <- pMany $ do
    tp <-
      pManySpaces
        >> pType
    name <- pIdentifier
    _ <-
      pManySpaces
        >> pOne ";" id
    return (tp, name)

  _ <-
    pManySpaces
      >> pOne "}" id
  return $
    Structure
      { structName = ident,
        structFields = pairs
      }

-- 'enum ActionChoices { GoLeft, GoRight, GoStraight, SitStill }'
pTypeEnum :: Parser STypeEnum
pTypeEnum = do
  _ <-
    pManySpaces
      >> pOne "enum" id
      >> pManySpaces
  enum_name <- pIdentifier
  _ <-
    pManySpaces
      >> pOne "{" id
      >> pManySpaces
  enum1 <- pIdentifier
  enums <-
    pOpt $
      pMany $
        pManySpaces
          >> pOne "," id
          >> pManySpaces
          >> pIdentifier

  _ <-
    pManySpaces
      >> pOne "}" id
  return $
    STypeEnum
      { ename = enum_name,
        eelems = enum1 : fromMaybe [] enums
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

-- pTypeWithBitLength tries to consume an identifier as two parts, the letter parts and the integer parts
-- for the types such as int8, int16, ... int256
-- and we support the standard types in solidity only here, the custom one won't be treated as this
-- it consumes the alpha letters from the beginning, and tries to parse the following parts as a integer
-- if it fails, it will treat the indentifier as a whole and parses a result without number part
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

pTypeSimple :: Parser SType
pTypeSimple = do
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
