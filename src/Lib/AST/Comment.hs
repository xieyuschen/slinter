module Lib.AST.Comment where

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
import Lib.AST.Model
import Lib.Parser
import Text.Read (Lexeme (String), readMaybe)

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
