module Lib.AST.Util where

import Control.Monad.Except (MonadError (throwError))
import Data.Text (Text)
import Lib.AST.Model (VisibilitySpecifier (..))
import Lib.Parser (Parser, pIdentifier, pManySpaces)

pVisibilitySpecifier :: Parser VisibilitySpecifier
pVisibilitySpecifier = do
  ident <- pManySpaces >> pIdentifier
  case toVisibilitySpecifier ident of
    Just specifier -> return specifier
    Nothing -> throwError "not a valid visible specifier"

toVisibilitySpecifier :: Text -> Maybe VisibilitySpecifier
toVisibilitySpecifier str =
  case str of
    "public" -> return VsPublic
    "private" -> return VsPrivate
    "internal" -> return VsInternal
    "external" -> return VsExternal
    _ -> Nothing
