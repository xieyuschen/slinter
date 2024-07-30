{-# LANGUAGE OverloadedStrings #-}

module Lib.AST.FunctionSpec (spec) where

import Control.Monad (forM_)
import Lib.AST.Function
import Lib.AST.Model
import Lib.Parser (runSParser)
import Lib.TestCommon (verifyParser)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  parseFunctionQuotedArgs
  parseFunctionModifiers
  parseFunctionReturnsClauseSpec
  parseFunctionSignatureSpec

parseFunctionSignatureSpec :: Spec
parseFunctionSignatureSpec = do
  let testCases =
        [ ( "function inc(string name) public pure { count += 1; }",
            Right
              Function
                { fReturnTyp = Nothing,
                  fargs =
                    [ FnDeclArg
                        { fnArgTp = STypeString,
                          fnArgName = Just "name",
                          fnArgLocation = Storage
                        }
                    ],
                  fnVisibility = FnPublic,
                  fnState = FnStatePure,
                  fnIsVirtual = False,
                  fname = FnNormal "inc"
                },
            ""
          ),
          ( "function inc(string) public pure { count += 1; }",
            Right
              Function
                { fReturnTyp = Nothing,
                  fargs =
                    [ FnDeclArg
                        { fnArgTp = STypeString,
                          fnArgName = Nothing,
                          fnArgLocation = Storage
                        }
                    ],
                  fnVisibility = FnPublic,
                  fnState = FnStatePure,
                  fnIsVirtual = False,
                  fname = FnNormal "inc"
                },
            ""
          ),
          ( "function inc(string name, uint256 new_name) internal view returns (uint256) { count += 1; }",
            Right
              Function
                { fReturnTyp = Just $ STypeUint 256,
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
                  fnVisibility = FnInternal,
                  fnState = FnStateView,
                  fnIsVirtual = False,
                  fname = FnNormal "inc"
                },
            ""
          ),
          ( "function inc(string memory name, uint256 calldata new_name) internal view returns (uint256) { count += 1; }",
            Right
              Function
                { fReturnTyp = Just $ STypeUint 256,
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
                  fnVisibility = FnInternal,
                  fnState = FnStateView,
                  fnIsVirtual = False,
                  fname = FnNormal "inc"
                },
            ""
          ),
          ( "function inc() external payable returns (uint256) { count += 1; } }",
            Right
              Function
                { fReturnTyp = Just $ STypeUint 256,
                  fargs = [],
                  fnVisibility = FnExternal,
                  fnState = FnStatePayable,
                  fnIsVirtual = False,
                  fname = FnNormal "inc"
                },
            " }"
          ),
          ( "function fallback() external payable returns (uint256) { count += 1; } }",
            Right
              Function
                { fReturnTyp = Just $ STypeUint 256,
                  fargs = [],
                  fnVisibility = FnExternal,
                  fnState = FnStatePayable,
                  fnIsVirtual = False,
                  fname = FnFallback
                },
            " }"
          ),
          ( "function receive() external returns (uint256) { count += 1; } }",
            Right
              Function
                { fReturnTyp = Just $ STypeUint 256,
                  fargs = [],
                  fnVisibility = FnExternal,
                  fnState = FnStateDefault,
                  fnIsVirtual = False,
                  fname = FnReceive
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
                  fnIsVirtual = True,
                  fargs = [],
                  fReturnTyp = Just (STypeUint 256)
                },
            " }"
          )
        ]
  forM_ testCases $ verifyParser "function" pFunction

parseFunctionQuotedArgs :: Spec
parseFunctionQuotedArgs = do
  let testCases =
        [ ( "()",
            Right [],
            ""
          ),
          ( " (  ) ",
            Right [],
            ""
          ),
          ( " ( uint256 name) ",
            Right
              [ FnDeclArg
                  { fnArgTp = STypeUint 256,
                    fnArgName = Just "name",
                    fnArgLocation = Storage
                  }
              ],
            ""
          ),
          ( "(uint256 name, string new_name)",
            Right
              [ FnDeclArg
                  { fnArgTp = STypeUint 256,
                    fnArgName = Just "name",
                    fnArgLocation = Storage
                  },
                FnDeclArg
                  { fnArgTp = STypeString,
                    fnArgName = Just "new_name",
                    fnArgLocation = Storage
                  }
              ],
            ""
          ),
          ( "(uint256 memory name, string calldata new_name, string)",
            Right
              [ FnDeclArg
                  { fnArgTp = STypeUint 256,
                    fnArgName = Just "name",
                    fnArgLocation = Memory
                  },
                FnDeclArg
                  { fnArgTp = STypeString,
                    fnArgName = Just "new_name",
                    fnArgLocation = Calldata
                  },
                FnDeclArg
                  { fnArgTp = STypeString,
                    fnArgName = Nothing,
                    fnArgLocation = Storage
                  }
              ],
            ""
          ),
          ( " ( uint256 name, string old_name, fixed256x16) ",
            Right
              [ FnDeclArg
                  { fnArgTp = STypeUint 256,
                    fnArgName = Just "name",
                    fnArgLocation = Storage
                  },
                FnDeclArg
                  { fnArgTp = STypeString,
                    fnArgName = Just "old_name",
                    fnArgLocation = Storage
                  },
                FnDeclArg
                  { fnArgTp = STypeFixed 256 16,
                    fnArgName = Nothing,
                    fnArgLocation = Storage
                  }
              ],
            ""
          )
        ]

  forM_ testCases $ verifyParser "function args quoted" pFunctionArgsInParentheses

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
  forM_ testCases $ verifyParser "function decorator" pFunctionDecorators

parseFunctionReturnsClauseSpec :: Spec
parseFunctionReturnsClauseSpec = do
  describe "parse function modifiers with curly bracket and returns" $ do
    let str = "returns (uint256){"
    let (result, s) = runSParser pReturnsClause str
    it "could parse the args" $ do
      result `shouldBe` Right (STypeUint 256)
    it "should leave correct state" $ do
      s `shouldBe` "{"
