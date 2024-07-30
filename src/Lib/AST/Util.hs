{-# LANGUAGE OverloadedStrings #-}

module Lib.AST.Util where

import Data.Text (Text)
import Lib.AST.Model (FnVisibility (..))
import Lib.Parser (Parser, pIdentifier, pManySpaces)

pFnVisibility :: Parser FnVisibility
pFnVisibility = do
  ident <- pManySpaces >> pIdentifier
  case toFnVisibility ident of
    Just specifier -> return specifier
    Nothing -> fail "not a valid visible specifier"

toFnVisibility :: Text -> Maybe FnVisibility
toFnVisibility str =
  case str of
    "public" -> return FnPublic
    "private" -> return FnPrivate
    "internal" -> return FnInternal
    "external" -> return FnExternal
    _ -> Nothing
