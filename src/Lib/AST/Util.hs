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
import Lib.AST.Expr (pLocationModifier)
import Lib.AST.Model
import Lib.AST.Type
import Lib.Parser
import Text.Parsec

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

-- we put it into util to avoid statement and function import each other
-- parse the visibility of function definition
pFnDeclVisibility :: Parser FnVisibility
pFnDeclVisibility =
  do
    try (pOneKeyword "public") $> FnPublic
    <|> try (pOneKeyword "private") $> FnPrivate
    <|> try (pOneKeyword "internal") $> FnInternal
    <|> try (pOneKeyword "external") $> FnExternal
