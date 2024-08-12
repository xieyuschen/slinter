{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}

module Lib.AST.Model where

import Data.Text (Text)
import Lib.Parser (SemVer)
import SumTypes.TH

keywordLogicalOr :: Text
keywordLogicalOr = "||"

keywordLogicalAnd :: Text
keywordLogicalAnd = "&&"

keywordEquality :: Text
keywordEquality = "=="

keywordLogicalNegation :: Text
keywordLogicalNegation = "!"

keywordInequality :: Text
keywordInequality = "!="

keywordContract :: Text
keywordContract = "contract"

keywordPragma :: Text
keywordPragma = "pragma"

keywordSolidity :: Text
keywordSolidity = "solidity"

keywordCommentPrefix :: Text
keywordCommentPrefix = "//"

semicolon :: Text
semicolon = ";"

colon :: Text
colon = ":"

leftCurlyBrace :: Text
leftCurlyBrace = "{"

rightCurlyBrace :: Text
rightCurlyBrace = "}"

leftParenthesis :: Text
leftParenthesis = "("

rightParenthesis :: Text
rightParenthesis = ")"

leftSquareBracket :: Text
leftSquareBracket = "["

rightSquareBracket :: Text
rightSquareBracket = "]"

-- // SPDX-License-Identifier: MIT
type SPDXComment = Text

-- // compiler version must be greater than or equal to 0.8.24 and less than 0.9.0
type Comment = Text

-- pragma solidity ^0.8.24;
type Pragma = SemVer

data Structure = Structure
  { structName :: Text,
    structFields :: [(SType, Text)]
  }
  deriving (Show, Eq)

data Mapping = Mapping
  { mKeyType :: SType,
    mValueType :: SType
  }
  deriving (Show, Eq)

data UserDefinedValueTypeDefinition = UserDefinedValueTypeDefinition
  { userDefinedValueTypeName :: Text,
    userDefinedValueElemType :: SType
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
  | STypeCustom Text
  | STypeAlias UserDefinedValueTypeDefinition
  | STypeStructure Structure
  | CustomTODO
  deriving (Show, Eq)

data ArrayN = ArrayN
  { aElemType :: SType,
    aSize :: Maybe SExpr -- nothing means the dynamic size
  }
  deriving (Show, Eq)

data FnName
  = FnNormal Text
  | FnFallback -- function fallback
  | FnReceive -- function receive
  deriving (Show, Eq)

data FunctionDefinition = FunctionDefinition
  { fnDefName :: FnName,
    fnState :: FnStateMutability,
    fnVisibility :: FnVisibility,
    fnModifierInvocations :: [FnModifierInvocation],
    fnFnOverrideSpecifier :: Maybe OverrideSpecifier,
    fnIsVirtual :: Bool,
    fargs :: [FnDeclArg],
    fnReturnTyp :: Maybe SType,
    fnBody :: Maybe [Stat] -- Nothing refers to a function declaration without body
  }
  deriving (Show, Eq)

data FnModifierInvocation = FnModifierInvocation
  { fnModifierInvocationPath :: IdentifierPath,
    fnModifierInvocationArgs :: Maybe FnCallArgs
  }
  deriving (Show, Eq)

type OverrideSpecifier = [IdentifierPath]

data FnDecorator
  = FnDecV FnVisibility
  | FnDecS FnStateMutability
  | FnDecMI FnModifierInvocation
  | FnDecVirtual
  | FnDecOs OverrideSpecifier
  deriving (Show, Eq)

-- todo: the function is not necessary, remove them
extractFnDecV :: [FnDecorator] -> [FnVisibility]
extractFnDecV decorators = [v | FnDecV v <- decorators]

extractFnDecS :: [FnDecorator] -> [FnStateMutability]
extractFnDecS decorators = [v | FnDecS v <- decorators]

extractFnDecMI :: [FnDecorator] -> [FnModifierInvocation]
extractFnDecMI decorators = [v | FnDecMI v <- decorators]

extractFnDecOs :: [FnDecorator] -> [OverrideSpecifier]
extractFnDecOs decorators = [v | FnDecOs v <- decorators]

-- function Visibility
data FnVisibility
  = FnPublic -- could be accessed from within/derived contract and external
  | FnPrivate -- only the contract it's defined in
  | FnInternal -- within contract and from derived contracts
  | FnExternal -- can only be called outside the contract
  deriving (Show, Eq)

-- function state-mutability
data FnStateMutability
  = FnStatePure
  | FnStateView
  | FnStatePayable
  | FnStateDefault -- no explicitly specify the state
  deriving (Show, Eq)

data StateVariableConstrain
  = SVarPublic
  | SVarPrivate
  | SVarInternal
  | SVarConstant
  | SVarOs OverrideSpecifier
  | SVarImmutable
  | SVarTransient
  deriving (Show, Eq)

-- StateVariable is the state variable of a contract, which is stored in the blockchain
-- please differ it from the StVarDefStatement where it could be temporary one,
-- and the storage place is determined by its keyword such as 'memory'
data StateVariable = StateVariable
  { svConstrains :: [StateVariableConstrain],
    svType :: SType,
    svName :: Text,
    svComment :: Maybe Comment, -- attached comment
    svVarExpr :: Maybe SExpr
  }
  deriving (Show, Eq)

data BitLengthDesc
  = BitLength Int
  | BitLengthWithDecimal Int Int -- the first is the bit length and the second is the decimal length
  deriving (Show, Eq)

keywordFunction :: Text
keywordFunction = "function"

keywordReturns :: Text
keywordReturns = "returns"

data STypeEnum = STypeEnum
  { ename :: Text,
    eelems :: [Text]
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
  | LString Text
  deriving (Show, Eq)

data FnDeclArg = FnDeclArg
  { fnArgTp :: SType,
    fnArgName :: Maybe Text,
    fnArgLocation :: DataLocation
  }
  deriving (Show, Eq)

data FnCallArgs
  = FnCallArgsList [SExpr] -- refers to the normal function call
  -- refers to the 'named parameter' call
  -- see https://docs.soliditylang.org/en/latest/control-structures.html#function-calls-with-named-parameters
  | FnCallArgsNamedParameters [(Text, SExpr)]
  deriving (Show, Eq)

data ExprFnCall = ExprFnCall
  { -- Nothing refers to it's an internal function,
    -- 'Just c' refers to the external function call from contract c
    fnContractName :: Maybe Text,
    fnName :: Text,
    fnArguments :: FnCallArgs
  }
  deriving (Show, Eq)

data ExprSelection = ExprSelection
  { selectionBase :: SExpr,
    selectionField :: Text
  }
  deriving (Show, Eq)

data ExprIndex = ExprIndex
  { elemBase :: SExpr,
    elemIndex :: SExpr
  }
  deriving (Show, Eq)

data ExprTernary = ExprTernary
  { ternaryCond :: SExpr,
    leftTernaryExpr :: SExpr,
    rightTernaryExpr :: SExpr
  }
  deriving (Show, Eq)

data SExpr
  = SExprB ExprBinary
  | SExprParentheses SExpr
  | SExprU ExprUnary
  | SExprVar Text -- Text refers to the variable name
  | SExprL Literal
  | SExprF ExprFnCall
  | SExprS ExprSelection
  | SExprI ExprIndex
  | SExprT ExprTernary
  | SExprN ExprFnCall
  | SExprD Text -- delete keyword
  | SExprEnd
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
  | ComparisonLessEqual -- <=
  | ComparisonLess -- <
  | ComparisonMoreEqual -- >=
  | ComparisonMore -- >
  | BitAnd -- &
  | BitOr --  |
  | BitExor --  ^
  | BitNeg -- ~
  | ShiftLeft -- <<
  | ShiftRight -- >>
  | CompoundAddition -- +=
  | CompoundMinus -- -=
  | CompoundMultiply -- '*='
  | CompoundDivision -- /=
  | CompoundModulus -- %=
  | CompoundAnd -- '&='
  | CompoundOr -- '|='
  | CompoundExor -- '^='
  | CompoundLeftShift -- <<=
  | CompoundRightShift -- >>=
  | Increment -- ++
  | Decrement -- --
  deriving (Show, Eq)

data StAssignStatement = StAssignStatement
  { stAssignVarName :: Text,
    stAssignExpr :: SExpr
  }
  deriving (Show, Eq)

-- https://docs.soliditylang.org/en/latest/types.html#data-location
data DataLocation = Memory | Storage | Calldata
  deriving (Show, Eq)

-- StVarDefStatement is the normal variable, and the st means 'stat'
-- such as 'uint memory name = 1+2;'
data StVarDefStatement = StVarDefStatement
  { stVarType :: SType,
    stVarName :: Text,
    stVarLocation :: DataLocation,
    stVarExpr :: Maybe SExpr,
    stVarComment :: Maybe Comment -- attached comment
  }
  deriving (Show, Eq)

data IfStatement = IfStatement
  { stIfCond :: SExpr,
    stIfThen :: [Stat],
    stIfElse :: [Stat]
  }
  deriving (Show, Eq)

newtype BlockStatement = BlockStatement
  { blockStats :: [Stat]
  }
  deriving (Show, Eq)

data ForStatement = ForStatement
  { forDecl :: Maybe StVarDefStatement,
    forExprStat :: Maybe SExpr, -- expression might be empty
    forCond :: Maybe SExpr,
    forBody :: [Stat]
  }
  deriving (Show, Eq)

data WhileStatement = WhileStatement
  { whileCond :: SExpr,
    whileBody :: [Stat]
  }
  deriving (Show, Eq)

data DoWhileStatement = DoWhileStatement
  { doWhileBody :: [Stat],
    doWhileCond :: SExpr
  }
  deriving (Show, Eq)

-- try new Foo(_owner) returns (Foo foo) {
--     // you can use variable foo here
--     emit Log("Foo created");
-- } catch Error(string memory reason) {
--     // catch failing revert() and require()
--     emit Log(reason);
-- } catch (bytes memory reason) {
--     // catch failing assert()
--     emit LogBytes(reason);
-- }
data TryStatement = TryStatement
  { tryExpr :: SExpr,
    tryReturns :: [FnDeclArg],
    tryBody :: [Stat],
    tryCatches :: [CatchStatement]
  }
  deriving (Show, Eq)

data CatchStatement = CatchStatement
  { -- the identifier is optional
    catchIdent :: Maybe Text,
    catchParams :: [FnDeclArg],
    catchBody :: [Stat]
  }
  deriving (Show, Eq)

data EmitStatement = EmitStatement
  { emitEventIdent :: Text,
    emitCallArgs :: FnCallArgs
  }
  deriving (Show, Eq)

data RevertStatement = RevertStatement
  { revertEventIdent :: Text,
    revertCallArgs :: FnCallArgs
  }
  deriving (Show, Eq)

data Stat
  = StatAssign StAssignStatement
  | StatVarDef StVarDefStatement
  | StatExpr SExpr
  | StatIf IfStatement
  | StatFor ForStatement
  | StatWhile WhileStatement
  | StatBlock BlockStatement
  | StatDoWhile DoWhileStatement
  | StatContinue
  | StatBreak
  | StatTry TryStatement
  | StatReturn (Maybe SExpr)
  | StatEmit EmitStatement
  | StatRevert RevertStatement
  | StatAssmbly -- don't support assmbly yet
  deriving (Show, Eq)

type ImportedPath = Text

data ImportDirective
  = ImportPath ImportedPath
  | ImportPathWithAlias ImportedPath Text
  | -- the first is symbol-aliases and the second is path
    ImportSymbolAliases ImportedPath [SymbolAlias]
  | ImportAllSymbols ImportedPath Text
  deriving (Show, Eq)

-- unit '{ identifier as identifier }' in import directive
-- 'import { identifier as identifier } from path'
data SymbolAlias = SymbolAlias
  { symbolIdentifier :: Text,
    symbolAlias :: Maybe Text
  }
  deriving (Show, Eq)

type UserDefinableOperator = Operator

type IdentifierPath = [Text]

data UsingAlias = UsingAlias
  { uaIdentifierPath :: IdentifierPath,
    uaUserDefinableOper :: Maybe UserDefinableOperator
  }
  deriving (Show, Eq)

data UsingField
  = UFIdentifierPath [Text] -- a.b.c
  | UFUsingAliases [UsingAlias]
  deriving (Show, Eq)

data UsingType = UsingTp SType | UsingAll deriving (Show, Eq)

data UsingDirective = UsingDirective
  { usingDirectiveField :: UsingField,
    usingType :: UsingType,
    usingGlobal :: Bool
  }
  deriving (Show, Eq)

-- modifier definition
data ModifierDefinition = ModifierDefinition
  { modifierName :: Text,
    modifierParamList :: Maybe [FnDeclArg], -- modifier might not receive parameter list
    modifierIsVirtual :: Bool,
    modifierOverrideSpecifier :: Maybe OverrideSpecifier,
    modifierBody :: Maybe [Stat] -- Nothing refers the modifier without a body
  }
  deriving (Show, Eq)

data EventParameter = EventParameter
  { eventParamType :: SType,
    eventParamIndex :: Maybe Int,
    eventParamIdent :: Maybe Text
  }
  deriving (Show, Eq)

data EventDefinition = EventDefinition
  { eventName :: Text,
    eventParameters :: [EventParameter],
    eventIsAnonymous :: Bool
  }
  deriving (Show, Eq)

data ContractDefinition = ContractDefinition
  { contractIsAbstract :: Bool,
    contractName :: Text,
    contractInheritanceSpecifiers :: [InheritanceSpecifier],
    contractBody :: ContractBody
  }
  deriving (Show, Eq)

data ConstructorMutability
  = ConstructorPayable
  | ConstructorInternal
  | ConstructorPublic
  deriving (Show, Eq)

data ConstructorDefinition = ConstructorDefinition
  { constructorParamList :: [FnDeclArg],
    -- todo: check whether it could be multiple
    constructorModifierInvocation :: Maybe FnModifierInvocation,
    constructorMutability :: ConstructorMutability,
    constructorBody :: [Stat]
  }
  deriving (Show, Eq)

-- todo: impl me pls
data ErrorDefinition = ErrorDefinition
  { errName :: Text,
    errParameters :: [ErrorParameter]
  }
  deriving (Show, Eq)

data ErrorParameter = ErrorParameter
  { errParamType :: SType,
    errParamName :: Maybe Text
  }
  deriving (Show, Eq)

data InterfaceDefinition = InterfaceDefinition
  { interfaceName :: Text,
    interfaceInheritanceSpecifiers :: [InheritanceSpecifier],
    interfaceBody :: ContractBody
  }
  deriving (Show, Eq)

data LibraryDefinition = LibraryDefinition
  { libraryName :: Text,
    libraryBody :: ContractBody
  }
  deriving (Show, Eq)

data InheritanceSpecifier = InheritanceSpecifier
  { inheritancePath :: IdentifierPath,
    inheritanceCallArgs :: Maybe FnCallArgs
  }
  deriving (Show, Eq)

data ContractBodyField
  = CtFunction FunctionDefinition
  | CtVariable StateVariable
  | CtComment Comment
  | CtEmptyLine
  deriving (Show, Eq)

data ContractBody = ContractBody
  { ctBodyConstructor :: Maybe ConstructorDefinition,
    ctBodyFunctions :: [FunctionDefinition],
    ctBodyModifiers :: [ModifierDefinition],
    ctBodyFallbackFunctions :: [FunctionDefinition],
    ctBodyReceiveFunctions :: [FunctionDefinition],
    ctBodyStructDefinitions :: [Structure],
    ctBodyEnumDefinitions :: [STypeEnum],
    ctBodyUserDefinedValueTypeDefinition :: [UserDefinedValueTypeDefinition],
    ctBodyStateVariables :: [StateVariable],
    ctBodyEventDefinitions :: [EventDefinition],
    ctBodyErrorDefinitions :: [ErrorDefinition],
    ctBodyUsingDirectives :: [UsingDirective]
    -- todo: fix me: when using haskell template to generate the code,
    -- the code place in the file matters
    -- the generated CBFSSum type cannot be referenced at the code above the 'constructSumType'
    -- however, when i move the data definition below the other structures cannot visit 'ContractBody'
    --ctBodyAllFields :: [CBFSSum]
  }
  deriving (Show, Eq)

constructSumType "CBFSSum" defaultSumTypeOptions [
  ''ConstructorDefinition, ''FunctionDefinition, ''ModifierDefinition
  , ''Structure, ''STypeEnum, ''UserDefinedValueTypeDefinition
  , ''StateVariable, ''EventDefinition , ''ErrorDefinition 
  , ''UsingDirective , ''Comment
  ]

deriving instance Show CBFSSum
deriving instance Eq CBFSSum

-- SolFile stands all definitions and the filename of a sol file,
-- which typically ends with file extension `.sol`
data SolFile = SolFile {
  solFileName :: String, 
  solPragma :: Pragma,
  solSpdxLicense :: SPDXComment,
  solImprotDirectives :: [ImportDirective],
  solUsingDirectives :: [UsingDirective],
  solContracts :: [ContractDefinition],
  solInterfaces :: [InterfaceDefinition],
  solLibraries :: [LibraryDefinition],
  solFunctions :: [FunctionDefinition],
  solConstantVars :: [StateVariable],
  solStructs:: [Structure],
  solEnums :: [STypeEnum],
  solUserDefineValueType :: [UserDefinedValueTypeDefinition],
  solErrors :: [ErrorDefinition],
  solEvents :: [EventDefinition]
}  deriving (Show, Eq)