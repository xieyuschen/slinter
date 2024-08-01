{-# LANGUAGE OverloadedStrings #-}

module Lib.AST.Util where

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
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace
import Lib.AST.Model
import Lib.AST.Type
import Lib.Parser
import Text.Parsec

pLocationModifier :: Parser DataLocation
pLocationModifier =
  try (pOneKeyword "memory") $> Memory
    <|> try (pOneKeyword "storage") $> Storage
    <|> try (pOneKeyword "calldata") $> Calldata

-- we put it into util to avoid statement and function import each other
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

pStateVariableConstrain :: Parser StateVariableConstrain
pStateVariableConstrain =
  do
    try (pOneKeyword "public") $> SVarPublic
    <|> try (pOneKeyword "private") $> SVarPrivate
    <|> try (pOneKeyword "internal") $> SVarInternal
    <|> try (pOneKeyword "constant") $> SVarConstant
    <|> try (SVarOs <$> pOverrideSpecifier)
    <|> try (pOneKeyword "immutable") $> SVarImmutable
    <|> try (pOneKeyword "transient") $> SVarTransient

-- we put it into util to avoid statement and function import each other
-- parse the visibility of function definition
pFnDeclVisibility :: Parser FnVisibility
pFnDeclVisibility =
  do
    try (pOneKeyword "public") $> FnPublic
    <|> try (pOneKeyword "private") $> FnPrivate
    <|> try (pOneKeyword "internal") $> FnInternal
    <|> try (pOneKeyword "external") $> FnExternal

-- parse the '(name: uint)' as so on. it will consume the following spaces
pFnDeclArgsInParentheses :: Parser [FnDeclArg]
pFnDeclArgsInParentheses = do
  fmap (fromMaybe []) $
    pManySpaces
      >> pOneKeyword leftParenthesis
      >> pManySpaces
      >> optionMaybe pFunctionArgs
        <* ( pManySpaces
               >> pOneKeyword rightParenthesis
               >> pManySpaces
           )

-- whether the function is decorated by the 'virtual' keyword
pFnDeclVirtual :: Parser FnDecorator
pFnDeclVirtual = try (pOneKeyword "virtual") $> FnDecVirtual

pOverrideSpecifier :: Parser OverrideSpecifier
pOverrideSpecifier = do
  paths <-
    pOneKeyword "override"
      *> optionMaybe
        ( between
            (pOneKeyword leftParenthesis >> pManySpaces)
            (pManySpaces >> pOneKeyword rightParenthesis)
            (sepBy (pManySpaces *> pIdentifierPath <* pManySpaces) $ char ',')
        )
  case paths of
    Nothing -> return [] -- we encounter the key 'override', but the body is omitted
    Just l -> return l

pIdentifierPath :: Parser IdentifierPath
pIdentifierPath = sepBy1 pIdentifier (char '.')
