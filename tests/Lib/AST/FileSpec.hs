{-# LANGUAGE OverloadedStrings #-}

module Lib.AST.FileSpec (spec) where

import Data.Bifunctor (first)
import Data.Text (pack)
import Lib.AST.File (pWholeSolFile)
import Lib.AST.Model
  ( ContractBody
      ( ContractBody,
        ctBodyConstructor,
        ctBodyEnumDefinitions,
        ctBodyErrorDefinitions,
        ctBodyEventDefinitions,
        ctBodyFallbackFunctions,
        ctBodyFunctions,
        ctBodyModifiers,
        ctBodyReceiveFunctions,
        ctBodyStateVariables,
        ctBodyStructDefinitions,
        ctBodyUserDefinedValueTypeDefinition,
        ctBodyUsingDirectives
      ),
    ContractDefinition
      ( ContractDefinition,
        contractBody,
        contractInheritanceSpecifiers,
        contractIsAbstract,
        contractName
      ),
    ExprBinary (ExprBinary, bOperator, leftOperand, rightOperand),
    FnName (FnNormal),
    FnStateMutability (FnStateDefault, FnStateView),
    FnVisibility (FnPublic),
    FunctionDefinition
      ( FunctionDefinition,
        fargs,
        fnBody,
        fnDefName,
        fnFnOverrideSpecifier,
        fnIsVirtual,
        fnModifierInvocations,
        fnReturnTyp,
        fnState,
        fnVisibility
      ),
    Literal (LNum),
    Operator (CompoundAddition, CompoundMinus),
    SExpr (SExprB, SExprL, SExprVar),
    SType (STypeUint),
    SolFile
      ( SolFile,
        solConstantVars,
        solContracts,
        solEnums,
        solErrors,
        solEvents,
        solFileName,
        solFunctions,
        solImprotDirectives,
        solInterfaces,
        solLibraries,
        solPragma,
        solSpdxLicense,
        solStateVars,
        solStructs,
        solUserDefineValueType,
        solUsingDirectives
      ),
    Stat (StatComment, StatExpr, StatReturn),
    StateVariable
      ( StateVariable,
        svComment,
        svConstrains,
        svName,
        svType,
        svVarExpr
      ),
    StateVariableConstrain (SVarPublic),
  )
import Lib.AST.Parser (SemVer (..), SemVerRangeMark (..), runSParser)
import Test.Hspec (Spec, beforeAll, describe, it, shouldBe)
import Text.Parsec.Error (errorMessages, messageString)

spec :: Spec
spec = do
  parseFileSpec

-- todo: fix me: abstract me into testcommon
parseFileSpec :: Spec
parseFileSpec = beforeAll
  ( do
      content <- readFile "tests/Lib/testdata/test.sol"
      let parsed = runSParser (pWholeSolFile "tests/Lib/testdata/test.sol") $ pack content
      return parsed
  )
  $ describe "read and parse the sol file correctly"
  $ do
    it "parses with correct result" $ \(result, _) ->
      do
        first
          ( fmap pack
              . filter (not . null)
              . fmap messageString
              . errorMessages
          )
          result
        `shouldBe` Right
          ( SolFile
              { solFileName = "tests/Lib/testdata/test.sol",
                solPragma = SemVer {major = 0, minor = 8, patch = Just 24, semVerRangeMark = Just Caret},
                solSpdxLicense = "MIT",
                solImprotDirectives = [],
                solUsingDirectives = [],
                solContracts =
                  [ ContractDefinition
                      { contractIsAbstract = False,
                        contractName = "Counter",
                        contractInheritanceSpecifiers = [],
                        contractBody =
                          ContractBody
                            { ctBodyConstructor = Nothing,
                              ctBodyFunctions =
                                [ FunctionDefinition
                                    { fnDefName = FnNormal "get",
                                      fnState = FnStateView,
                                      fnVisibility = FnPublic,
                                      fnModifierInvocations = [],
                                      fnFnOverrideSpecifier = Nothing,
                                      fnIsVirtual = False,
                                      fargs = [],
                                      fnReturnTyp = Just (STypeUint 256),
                                      fnBody = Just [StatReturn (Just (SExprVar "count"))]
                                    },
                                  FunctionDefinition
                                    { fnDefName = FnNormal "inc",
                                      fnState = FnStateDefault,
                                      fnVisibility = FnPublic,
                                      fnModifierInvocations = [],
                                      fnFnOverrideSpecifier = Nothing,
                                      fnIsVirtual = False,
                                      fargs = [],
                                      fnReturnTyp = Nothing,
                                      fnBody =
                                        Just
                                          [ StatExpr
                                              ( SExprB
                                                  ExprBinary
                                                    { leftOperand = SExprVar "count",
                                                      rightOperand = SExprL (LNum 1),
                                                      bOperator = CompoundAddition
                                                    }
                                              )
                                          ]
                                    },
                                  FunctionDefinition
                                    { fnDefName = FnNormal "dec",
                                      fnState = FnStateDefault,
                                      fnVisibility = FnPublic,
                                      fnModifierInvocations = [],
                                      fnFnOverrideSpecifier = Nothing,
                                      fnIsVirtual = False,
                                      fargs = [],
                                      fnReturnTyp = Nothing,
                                      fnBody =
                                        Just
                                          [ StatComment " This function will fail if count = 0",
                                            StatExpr
                                              ( SExprB
                                                  ExprBinary
                                                    { leftOperand = SExprVar "count",
                                                      rightOperand = SExprL (LNum 1),
                                                      bOperator = CompoundMinus
                                                    }
                                              )
                                          ]
                                    }
                                ],
                              ctBodyModifiers = [],
                              ctBodyFallbackFunctions = [],
                              ctBodyReceiveFunctions = [],
                              ctBodyStructDefinitions = [],
                              ctBodyEnumDefinitions = [],
                              ctBodyUserDefinedValueTypeDefinition = [],
                              ctBodyStateVariables =
                                [ StateVariable
                                    { svConstrains = [SVarPublic],
                                      svType = STypeUint 256,
                                      svName = "count",
                                      svComment = Just " Function to get the current count",
                                      svVarExpr = Nothing
                                    }
                                ],
                              ctBodyEventDefinitions = [],
                              ctBodyErrorDefinitions = [],
                              ctBodyUsingDirectives = []
                            }
                      }
                  ],
                solInterfaces = [],
                solLibraries = [],
                solFunctions = [],
                solConstantVars = [],
                solStructs = [],
                solEnums = [],
                solStateVars = [],
                solUserDefineValueType = [],
                solErrors = [],
                solEvents = []
              }
          )

    it "parses with an empty remaining string" $ \(_, s) -> do
      s `shouldBe` ""
