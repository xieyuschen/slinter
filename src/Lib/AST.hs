module Lib.AST where 
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import Control.Monad.State
    
-- // SPDX-License-Identifier: MIT
type SPDXComment = String

-- // compiler version must be greater than or equal to 0.8.24 and less than 0.9.0
type Comment = String

-- pragma solidity ^0.8.24;
type Pragma = String

data Type = UInt256Type | StringType deriving (Show)

data Function = Function {
  fname :: String,
  fargs :: [String]
} deriving (Show)


data Modifier = Public | Private deriving (Show)

data Variable = Variable {
  vmodifier :: Modifier,
  vtyp :: Type,
  vname :: String
} deriving (Show)

data Contract = Contract {
  cfunctions :: [Function],
  cvariables :: [Variable]
} deriving (Show)

data AST
  = ASTSPDXComment SPDXComment
  | ASTComment Comment
  | ASTPragma Pragma
  | ASTType Type
  | ASTFunction Function
  | ASTModifier Modifier
  | ASTVariable Variable
  | ASTContract Contract
  deriving (Show)

-- TODO: replace MaybeT with ErrorT
parseAST :: String -> MaybeT (State AST) String
parseAST _ = MaybeT $ return Nothing

