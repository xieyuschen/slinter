{-# LANGUAGE OverloadedStrings #-}

module Lib.AST.FunctionSpec (spec) where

import Control.Monad (forM_)
import Lib.AST.Function
import Lib.AST.Model
import Lib.Parser (runSParser)
import Lib.TestCommon (exactlyParserVerifier)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  parseFunctionModifiers
  parseFunctionReturnsClauseSpec
  parseFunctionSignatureSpec

parseFunctionSignatureSpec :: Spec
parseFunctionSignatureSpec = do
  let testCases =
        [ ( "function inc(string name) public pure { count += 1; }",
            Right
              Function
                { fnReturnTyp = Nothing,
                  fargs =
                    [ FnDeclArg
                        { fnArgTp = STypeString,
                          fnArgName = Just "name",
                          fnArgLocation = Storage
                        }
                    ],
                  fnModifierInvocations = [],
                  fnFnOverrideSpecifier = Nothing,
                  fnVisibility = FnPublic,
                  fnState = FnStatePure,
                  fnIsVirtual = False,
                  fname = FnNormal "inc",
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
            ""
          ),
          ( "function inc(string) public pure { count += 1; }",
            Right
              Function
                { fnReturnTyp = Nothing,
                  fargs =
                    [ FnDeclArg
                        { fnArgTp = STypeString,
                          fnArgName = Nothing,
                          fnArgLocation = Storage
                        }
                    ],
                  fnModifierInvocations = [],
                  fnFnOverrideSpecifier = Nothing,
                  fnVisibility = FnPublic,
                  fnState = FnStatePure,
                  fnIsVirtual = False,
                  fname = FnNormal "inc",
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
            ""
          ),
          ( "function inc(string name, uint256 new_name) internal view returns (uint256) { count += 1; }",
            Right
              Function
                { fnReturnTyp = Just $ STypeUint 256,
                  fargs =
                    [ FnDeclArg
                        { fnArgTp = STypeString,
                          fnArgName = Just "name",
                          fnArgLocation = Storage
                        },
                      FnDeclArg
                        { fnArgTp = STypeUint 256,
                          fnArgName = Just "new_name",
                          fnArgLocation = Storage
                        }
                    ],
                  fnModifierInvocations = [],
                  fnFnOverrideSpecifier = Nothing,
                  fnVisibility = FnInternal,
                  fnState = FnStateView,
                  fnIsVirtual = False,
                  fname = FnNormal "inc",
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
            ""
          ),
          ( "function inc(string memory name, uint256 calldata new_name) internal view returns (uint256) { count += 1; }",
            Right
              Function
                { fnReturnTyp = Just $ STypeUint 256,
                  fargs =
                    [ FnDeclArg
                        { fnArgTp = STypeString,
                          fnArgName = Just "name",
                          fnArgLocation = Memory
                        },
                      FnDeclArg
                        { fnArgTp = STypeUint 256,
                          fnArgName = Just "new_name",
                          fnArgLocation = Calldata
                        }
                    ],
                  fnModifierInvocations = [],
                  fnFnOverrideSpecifier = Nothing,
                  fnVisibility = FnInternal,
                  fnState = FnStateView,
                  fnIsVirtual = False,
                  fname = FnNormal "inc",
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
            ""
          ),
          ( "function inc() external payable returns (uint256) { count += 1; } }",
            Right
              Function
                { fnReturnTyp = Just $ STypeUint 256,
                  fargs = [],
                  fnModifierInvocations = [],
                  fnFnOverrideSpecifier = Nothing,
                  fnVisibility = FnExternal,
                  fnState = FnStatePayable,
                  fnIsVirtual = False,
                  fname = FnNormal "inc",
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
            " }"
          ),
          ( "function fallback() external payable returns (uint256);",
            Right
              Function
                { fnReturnTyp = Just $ STypeUint 256,
                  fargs = [],
                  fnModifierInvocations = [],
                  fnFnOverrideSpecifier = Nothing,
                  fnVisibility = FnExternal,
                  fnState = FnStatePayable,
                  fnIsVirtual = False,
                  fname = FnFallback,
                  fnBody = Nothing
                },
            ""
          ),
          ( "function receive() external returns (uint256) { count += 1; } }",
            Right
              Function
                { fnReturnTyp = Just $ STypeUint 256,
                  fargs = [],
                  fnModifierInvocations = [],
                  fnFnOverrideSpecifier = Nothing,
                  fnVisibility = FnExternal,
                  fnState = FnStateDefault,
                  fnIsVirtual = False,
                  fname = FnReceive,
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
            " }"
          ),
          ( -- todo: check whether it's valid if no decorator is used
            "function inc() virtual returns (uint256) { count += 1; } }",
            Right
              Function
                { fname = FnNormal "inc",
                  fnVisibility = FnInternal,
                  fnState = FnStateDefault,
                  fnModifierInvocations = [],
                  fnFnOverrideSpecifier = Nothing,
                  fnIsVirtual = True,
                  fargs = [],
                  fnReturnTyp = Just (STypeUint 256),
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
            " }"
          ),
          ( "function inc() virtual modifierInvocation1 modifierInvocation1(owner) returns (uint256) ;",
            Right
              Function
                { fname = FnNormal "inc",
                  fnVisibility = FnInternal,
                  fnState = FnStateDefault,
                  fnModifierInvocations =
                    [ FnModifierInvocation
                        { fnModifierInvocationPath = ["modifierInvocation1"],
                          fnModifierInvocationArgs = Nothing
                        },
                      FnModifierInvocation
                        { fnModifierInvocationPath = ["modifierInvocation1"],
                          fnModifierInvocationArgs = Just (FnCallArgsList [SExprVar "owner"])
                        }
                    ],
                  fnFnOverrideSpecifier = Nothing,
                  fnIsVirtual = True,
                  fargs = [],
                  fnReturnTyp = Just (STypeUint 256),
                  fnBody = Nothing
                },
            ""
          ),
          ( "function inc() override returns (uint256) { count += 1; } }",
            Right
              Function
                { fname = FnNormal "inc",
                  fnVisibility = FnInternal,
                  fnState = FnStateDefault,
                  fnModifierInvocations = [],
                  fnFnOverrideSpecifier = Just [],
                  fnIsVirtual = False,
                  fargs = [],
                  fnReturnTyp = Just (STypeUint 256),
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
            " }"
          ),
          ( "function inc() override(a.b.c, a.b) returns (uint256) { count += 1; } }",
            Right
              Function
                { fname = FnNormal "inc",
                  fnVisibility = FnInternal,
                  fnState = FnStateDefault,
                  fnModifierInvocations = [],
                  fnFnOverrideSpecifier = Just [["a", "b", "c"], ["a", "b"]],
                  fnIsVirtual = False,
                  fargs = [],
                  fnReturnTyp = Just (STypeUint 256),
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
            " }"
          ),
          ( "function inc() virtual modifierInvocation1 override(a.b.c, a.b) modifierInvocation1(owner) returns (uint256);",
            Right
              Function
                { fname = FnNormal "inc",
                  fnVisibility = FnInternal,
                  fnState = FnStateDefault,
                  fnModifierInvocations =
                    [ FnModifierInvocation
                        { fnModifierInvocationPath = ["modifierInvocation1"],
                          fnModifierInvocationArgs = Nothing
                        },
                      FnModifierInvocation
                        { fnModifierInvocationPath = ["modifierInvocation1"],
                          fnModifierInvocationArgs = Just (FnCallArgsList [SExprVar "owner"])
                        }
                    ],
                  fnFnOverrideSpecifier = Just [["a", "b", "c"], ["a", "b"]],
                  fnIsVirtual = True,
                  fargs = [],
                  fnReturnTyp = Just (STypeUint 256),
                  fnBody = Nothing
                },
            ""
          )
        ]
  forM_ testCases $ exactlyParserVerifier "whole function" pFunction

parseFunctionModifiers :: Spec
parseFunctionModifiers = do
  let testCases =
        [ ( "public  view ",
            Right [FnDecV FnPublic, FnDecS FnStateView],
            ""
          ),
          ( "public  view {",
            Right [FnDecV FnPublic, FnDecS FnStateView],
            "{"
          ),
          ( "public {",
            Right [FnDecV FnPublic],
            "{"
          ),
          ( "public  view returns {",
            Right [FnDecV FnPublic, FnDecS FnStateView],
            "returns {"
          )
        ]
  forM_ testCases $ exactlyParserVerifier "function decorator" pFunctionDecorators

parseFunctionReturnsClauseSpec :: Spec
parseFunctionReturnsClauseSpec = do
  describe "parse function modifiers with curly bracket and returns" $ do
    let str = "returns (uint256){"
    let (result, s) = runSParser pReturnsClause str
    it "could parse the args" $ do
      result `shouldBe` Right (STypeUint 256)
    it "should leave correct state" $ do
      s `shouldBe` "{"
