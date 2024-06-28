module Lib.AST where

import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.RWS (MonadReader (ask))
import Control.Monad.State
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import Data.Char (isSpace)
import Lib.Parser
  ( Parser,
    SemVer,
    pMany,
    pOne,
    pOneKeyword,
    pReadline,
    pUntil,
    pIdentifier,
    parseManySpaces,
    pSemVer,
    pSpace,
  )

-- // SPDX-License-Identifier: MIT
type SPDXComment = String

-- // compiler version must be greater than or equal to 0.8.24 and less than 0.9.0
type Comment = String

-- pragma solidity ^0.8.24;
type Pragma = SemVer

data Type = UInt256Type | StringType deriving (Show, Eq)

data Function = Function
  { fname :: String,
    fargs :: [String]
  }
  deriving (Show, Eq)

data Modifier = Public | Private deriving (Show, Eq)

data Variable = Variable
  { vmodifier :: Modifier,
    vtyp :: Type,
    vname :: String
  }
  deriving (Show, Eq)

data Contract = Contract
  { cfunctions :: [Function],
    cvariables :: [Variable]
  }
  deriving (Show, Eq)

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
pSPDXComment :: Parser AST
pSPDXComment = do
  _ <- pOneKeyword "// SPDX-License-Identifier:"
  _ <- parseManySpaces
  content <- pReadline
  let (license, _) = break isSpace content
  return (ASTSPDXComment license)

-- // helloworld
pComment :: Parser AST
pComment = do
  _ <- pOneKeyword "//"
  ASTComment <$> pReadline

-- pragma solidity ^0.8.24;
pPragma :: Parser AST
pPragma = do
  _ <- parseManySpaces
  _ <- pOneKeyword "pragma"
  _ <- parseManySpaces
  _ <- pOneKeyword "solidity"
  _ <- parseManySpaces
  version <- pSemVer
  _ <- pOneKeyword ";"
  return (ASTPragma version)
