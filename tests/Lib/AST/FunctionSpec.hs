{-# LANGUAGE OverloadedStrings #-}

module Lib.AST.FunctionSpec (spec) where

import Control.Monad
import Lib.AST.Function
import Lib.AST.Model
import Lib.Parser
import Lib.TestCommon
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  parseFunctionQuotedArgs
  parseFunctionModifiers
  parseFunctionReturnsClauseSpec
  parseFunctionArgsSpec
  parseFunctionSignatureSpec

parseFunctionSignatureSpec :: Spec
parseFunctionSignatureSpec = do
  let testCases =
        [ ( "function inc(string name) public pure { count += 1; }",
            Right
              Function
                { fReturnTyp = Nothing,
                  fargs = [(STypeString, Just "name")],
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
                  fargs = [(STypeString, Nothing)],
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
                    [ (STypeString, Just "name"),
                      (STypeUint 256, Just "new_name")
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
            Left "visibility specifier should contain only one for each function",
            "returns (uint256) { count += 1; } }"
          )
        ]
  forM_ testCases $ verifyParser "function" pFunction

parseFunctionArgsSpec :: Spec
parseFunctionArgsSpec = do
  let testCases =
        [ ("uint256 name", Right [(STypeUint 256, Just "name")], ""),
          ( "uint256 name, string new_name",
            Right
              [ (STypeUint 256, Just "name"),
                (STypeString, Just "new_name")
              ],
            ""
          )
        ]
  forM_ testCases $ verifyParser "parse function args" pFunctionArgs

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
            Right [(STypeUint 256, Just "name")],
            ""
          ),
          ( " ( uint256 name, string old_name, fixed256x16) ",
            Right
              [ (STypeUint 256, Just "name"),
                (STypeString, Just "old_name"),
                (STypeFixed 256 16, Nothing)
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
    let (result, s) = runParser pReturnsClause str
    it "could parse the args" $ do
      result `shouldBe` Right (STypeUint 256)
    it "should leave correct state" $ do
      s `shouldBe` "{"
