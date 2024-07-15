module Lib.AST.Util where

import Control.Monad.Except (MonadError (throwError))
import Lib.AST.Model
import Lib.Parser

pVisibilitySpecifier :: Parser VisibilitySpecifier
pVisibilitySpecifier = do
  ident <- pManySpaces >> pIdentifier
  case toVisibilitySpecifier ident of
    Just specifier -> return specifier
    Nothing -> throwError "not a valid visible specifier"

toVisibilitySpecifier :: String -> Maybe VisibilitySpecifier
toVisibilitySpecifier str =
  case str of
    "public" -> return VsPublic
    "private" -> return VsPrivate
    "internal" -> return VsInternal
    "external" -> return VsExternal
    _ -> Nothing
