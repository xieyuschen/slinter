{-# LANGUAGE OverloadedStrings #-}

module Lib.AST.FunctionSpec (spec) where

import Control.Monad (forM_)
import Lib.AST.Function
import Lib.AST.Model
import Lib.Parser
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
        [ ( "function inc(string name) public views { count += 1; }",
            Right
              Function
                { fReturnTyp = Nothing,
                  fargs = [(STypeString, "name")],
                  fVisiblitySpecifier = VsPublic,
                  fmodifiers = ["views"],
                  fname = "inc"
                },
            ""
          ),
          ( "function inc(string name, uint256 new_name) views private returns (uint256) { count += 1; }",
            Right
              Function
                { fReturnTyp = Just $ STypeUint 256,
                  fargs = [(STypeString, "name"), (STypeUint 256, "new_name")],
                  fVisiblitySpecifier = VsPrivate,
                  fmodifiers = ["views"],
                  fname = "inc"
                },
            ""
          ),
          ( "function inc() internal returns (uint256) { count += 1; } }",
            Right
              Function
                { fReturnTyp = Just $ STypeUint 256,
                  fargs = [],
                  fVisiblitySpecifier = VsInternal,
                  fmodifiers = [],
                  fname = "inc"
                },
            " }"
          ),
          ( "function inc() returns (uint256) { count += 1; } }",
            Left "visibility specifier should contain only one for each function",
            "returns (uint256) { count += 1; } }"
          )
        ]
  forM_ testCases $ \(input, expectedResult, expectedState) -> do
    describe ("parse function: " ++ input) $ do
      let (result, s) = runParser pFunction input
      it "gets the correct function" $ do
        result `shouldBe` expectedResult
      it "leaves the correct state" $ do
        s `shouldBe` expectedState

parseFunctionArgsSpec :: Spec
parseFunctionArgsSpec = do
  describe "parse empty arg with quotes" $ do
    let str = "uint256 name"
    let (result, s) = runParser pFunctionArgs str
    it "could parse the args" $ do
      result `shouldBe` Right [(STypeUint 256, "name")]
    it "should leave correct state" $ do
      s `shouldBe` ""
  describe "parse empty arg with quotes" $ do
    let str = "uint256 name, string newname"
    let (result, s) = runParser pFunctionArgs str
    it "could parse the args" $ do
      result
        `shouldBe` Right
          [ (STypeUint 256, "name"),
            (STypeString, "newname")
          ]
    it "should leave correct state" $ do
      s `shouldBe` ""

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
            Right [(STypeUint 256, "name")],
            ""
          ),
          ( " ( uint256 name, string oldname) ",
            Right [(STypeUint 256, "name"), (STypeString, "oldname")],
            ""
          )
        ]
  forM_ testCases $ \(input, expectedResult, expectedState) -> do
    describe ("parse function args: " ++ input) $ do
      let (result, s) = runParser pFunctionArgsQuoted input
      it "could parse the args" $ do
        result `shouldBe` expectedResult
      it "should leave correct state" $ do
        s `shouldBe` expectedState

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
  forM_ testCases $ \(input, expectedResult, expectedState) -> do
    describe ("parse function modifiers: " ++ input) $ do
      let (result, s) = runParser pFunctionDecorators input
      it "could parse the modifiers" $ do
        result `shouldBe` expectedResult
      it "should leave correct state" $ do
        s `shouldBe` expectedState

parseFunctionReturnsClauseSpec :: Spec
parseFunctionReturnsClauseSpec = do
  describe "parse function modifiers with curly bracket and returns" $ do
    let str = "returns (uint256){"
    let (result, s) = runParser pReturnsClause str
    it "could parse the args" $ do
      result `shouldBe` Right (STypeUint 256)
    it "should leave correct state" $ do
      s `shouldBe` "{"
