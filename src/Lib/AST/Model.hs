module Lib.AST.Model where

import Lib.Parser (SemVer)

keywordLogicalOr :: String
keywordLogicalOr = "||"

keywordLogicalAnd :: String
keywordLogicalAnd = "&&"

keywordEquality :: String
keywordEquality = "=="

keywordLogicalNegation :: String
keywordLogicalNegation = "!"

keywordInequality :: String
keywordInequality = "!="

keywordContract :: String
keywordContract = "contract"

keywordPragma :: String
keywordPragma = "pragma"

keywordSolidity :: String
keywordSolidity = "solidity"

keywordCommentPrefix :: String
keywordCommentPrefix = "//"

semicolon :: String
semicolon = ";"

leftCurlyBrace :: String
leftCurlyBrace = "{"

rightCurlyBrace :: String
rightCurlyBrace = "}"

leftParenthesis :: String
leftParenthesis = "("

rightParenthesis :: String
rightParenthesis = ")"

leftSquareBracket :: String
leftSquareBracket = "["

rightSquareBracket :: String
rightSquareBracket = "]"

data ContractField
  = CtFunction Function
  | CtVariable StateVariable
  | CtComment Comment
  | CtEmptyLine
  deriving (Eq)

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
type SPDXComment = String

-- // compiler version must be greater than or equal to 0.8.24 and less than 0.9.0
type Comment = String

-- pragma solidity ^0.8.24;
type Pragma = SemVer

data Structure = Structure
  { structName :: String,
    structFields :: [(SType, String)]
  }
  deriving (Show, Eq)

data Mapping = Mapping
  { mKeyType :: SType,
    mValueType :: SType
  }
  deriving (Show, Eq)

data SAlias = SAlias
  { salias :: String,
    saliasOriginType :: SType
  }
  deriving (Show, Eq)

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
  | STypeMapping Mapping
  | STypeArray ArrayN
  | STypeCustom String
  | STypeAlias SAlias
  | STypeStructure Structure
  | CustomTODO
  deriving (Show, Eq)

data ArrayN = ArrayN
  { aElemType :: SType,
    aSize :: Maybe SExpr -- nothing means the dynamic size
  }
  deriving (Show, Eq)

data Function = Function
  { fmodifiers :: [String],
    fVisiblitySpecifier :: VisibilitySpecifier,
    fname :: String,
    fargs :: [(SType, Maybe String)],
    fReturnTyp :: Maybe SType
  }
  deriving (Show, Eq)

data VisibilitySpecifier
  = VsPublic -- could be accessed from within/derived contract and external
  | VsPrivate -- only the contract it's defined in
  | VsInternal -- within contract and from derived contracts
  | VsExternal -- can only be called outside the contract
  deriving (Show, Eq)

data StateVariable = StateVariable
  { svVisibleSpecifier :: VisibilitySpecifier,
    svType :: SType,
    svName :: String,
    svComment :: Maybe Comment -- attached comment
  }
  deriving (Show, Eq)

data Contract = Contract
  { ctName :: String,
    ctFunctions :: [Function],
    ctVariables :: [StateVariable]
  }
  deriving (Show, Eq)

data BitLengthDesc
  = BitLength Int
  | BitLengthWithDecimal Int Int -- the first is the bit length and the second is the decimal length
  deriving (Show, Eq)

keywordFunction :: String
keywordFunction = "function"

keywordReturns :: String
keywordReturns = "returns"

data STypeEnum = STypeEnum
  { ename :: String,
    eelems :: [String]
  }
  deriving (Show, Eq)

-- expression stands for a bool expression by now, for example 'a&&b' and 'a||b'
data ExprBinary = ExprBinary
  { leftOperand :: SExpr,
    rightOperand :: SExpr,
    bOperator :: Operator
  }
  deriving (Show, Eq)

data ExprUnary = ExprUnary
  { -- todo: consider to make Operator a sum-type then split the unary operator out
    uOperator :: Operator,
    uOperand :: SExpr
  }
  deriving (Show, Eq)

data Literal
  = LNum Int
  | LBool Bool
  | LString String
  deriving (Show, Eq)

data FnCallArgs
  = FnCallArgsList [SExpr] -- refers to the normal function call
  -- refers to the 'named parameter' call
  -- see https://docs.soliditylang.org/en/latest/control-structures.html#function-calls-with-named-parameters
  | FnCallArgsNamedParameters [(String, SExpr)]
  deriving (Show, Eq)

data ExprFnCall = ExprFnCall
  { -- Nothing refers to it's an internal function,
    -- 'Just c' refers to the external function call from contract c
    fnContractName :: Maybe String,
    fnName :: String,
    fnArguments :: FnCallArgs
  }
  deriving (Show, Eq)

data SExpr
  = SExprB ExprBinary
  | SExprParentheses SExpr
  | SExprU ExprUnary
  | SExprVar String -- String refers to the variable name
  | SExprL Literal
  | SExprF ExprFnCall
  deriving (Show, Eq)

data Operator
  = LogicalAnd -- &&
  | LogicalOr --  ||
  | LogicalNegation -- !
  | LogicalEqual -- ==
  | LogicalInequal -- !=
  | ArithmeticAddition -- +
  | Minus -- -, could be binary or unary operator
  | ArithmeticMultiplication
  | ArithmeticDivision -- /
  | ArithmeticModulus -- %
  | ArithmeticExp --  **
  | ComparisionLessEqual -- <=
  | ComparisionLess -- <
  | ComparisionMoreEqual -- >=
  | ComparisionMore -- >
  | BitAnd -- &
  | BitOr --  |
  | BitExor --  ^
  | BitNeg -- ~
  | ShiftLeft -- <<
  | ShiftRight -- >>
  | Assign -- =
  deriving (Show, Eq)