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
                  fVisiblitySpecifier = VsPublic,
                  fmodifiers = ["pure"],
                  fname = "inc"
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
                  fVisiblitySpecifier = VsPublic,
                  fmodifiers = ["pure"],
                  fname = "inc"
                },
            ""
          ),
          ( "function inc(string name, uint256 new_name) internal views returns (uint256) { count += 1; }",
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
                  fVisiblitySpecifier = VsInternal,
                  fmodifiers = ["views"],
                  fname = "inc"
                },
            ""
          ),
          ( "function inc(string memory name, uint256 calldata new_name) internal views returns (uint256) { count += 1; }",
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
                  fVisiblitySpecifier = VsInternal,
                  fmodifiers = ["views"],
                  fname = "inc"
                },
            ""
          ),
          ( "function inc() external payable returns (uint256) { count += 1; } }",
            Right
              Function
                { fReturnTyp = Just $ STypeUint 256,
                  fargs = [],
                  fVisiblitySpecifier = VsExternal,
                  fmodifiers = ["payable"],
                  fname = "inc"
                },
            " }"
          ),
          ( "function inc() returns (uint256) { count += 1; } }",
            Left ["\"r\"", "\"r\"", "space", "space", "visibility specifier should contain only one for each function"],
            "function inc() returns (uint256) { count += 1; } }"
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

  forM_ testCases $ verifyParser "function args quoted" pFunctionArgsQuoted

parseFunctionModifiers :: Spec
parseFunctionModifiers = do
  let testCases =
        [ ( "public  view",
            Right ["public", "view"],
            ""
          ),
          ( "public  view {",
            Right ["public", "view"],
            "{"
          ),
          ( "public {",
            Right ["public"],
            "{"
          ),
          ( "public  view returns {",
            Right ["public", "view"],
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
