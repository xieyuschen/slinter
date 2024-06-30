{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Lib.ASTSpec (spec) where

-- export multiple test functions is not supported, as the document reads:
-- Each spec file has to export a top-level binding spec of type Spec.
import Lib.AST
import Lib.Parser
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  parseFunctionSpec
  parseCommentSpec
  parseSPDXCommentSpec
  parsePragmaSpec

parseFunctionSpec :: Spec
parseFunctionSpec = do
  parseFunctionQuotedArgs
  parseFunctionModifiers
  parseReturnsClosureSpec
  parseFunctionArgsSpec
  describe "parse simple function" $ do
    let fnStr = "\
    \ function inc() public {\ 
    \    count += 1; \
    \}"
    let (result, s) = runParser pFunction fnStr
    it "get the correct function" $ do 
      result `shouldBe` Right Function { 
        fReturnTyp = Nothing,
        fargs = [],
        fmodifiers = ["public"],
        fname = "inc"
      }
    it "remain the correct state" $ do
      s `shouldBe` ""

  describe "parse simple function with return value" $ do
    let fnStr = "\
    \ function inc() public returns (uint256) {\ 
    \    count += 1; \
    \}"
    let (result, s) = runParser pFunction fnStr
    it "get the correct function" $ do 
      result `shouldBe` Right Function { 
        fReturnTyp = Just UInt256Type,
        fargs = [],
        fmodifiers = ["public"],
        fname = "inc"
      }
    it "remain the correct state" $ do
      s `shouldBe` ""

parseFunctionArgsSpec :: Spec
parseFunctionArgsSpec = do
  describe "parse empty arg with quotes" $ do
    let str = "uint256 name"
    let (result, s) = runParser pFunctionArgs str
    it "could parse the args" $ do
      result `shouldBe` Right [(UInt256Type, "name")]
    it "should leave correct state" $ do
      s `shouldBe` ""  
  describe "parse empty arg with quotes" $ do
    let str = "uint256 name, string newname"
    let (result, s) = runParser pFunctionArgs str
    it "could parse the args" $ do
      result `shouldBe` Right [
        (UInt256Type, "name"), (StringType, "newname")]
    it "should leave correct state" $ do
      s `shouldBe` ""  

parseFunctionQuotedArgs :: Spec
parseFunctionQuotedArgs = do
  describe "parse empty arg with quotes" $ do
    let str = "()"
    let (result, s) = runParser pFunctionArgsQuoted str
    it "could parse the args" $ do
      result `shouldBe` Right []
    it "should leave correct state" $ do
      s `shouldBe` ""
  describe "arse empty arg with quotes" $ do
    let str = " (  ) "
    let (result, s) = runParser pFunctionArgsQuoted str
    it "could parse the args" $ do
      result `shouldBe` Right []
    it "should leave correct state" $ do
      s `shouldBe` ""
  describe "parse one args with quotes" $ do
    let str = " ( uint256 name) "
    let (result, s) = runParser pFunctionArgsQuoted str
    it "could parse the args" $ do
      result `shouldBe` Right [(UInt256Type, "name")]
    it "should leave correct state" $ do
      s `shouldBe` ""
  describe "parse two args with quotes" $ do
    let str = " ( uint256 name, string oldname) "
    let (result, s) = runParser pFunctionArgsQuoted str
    it "could parse the args" $ do
      result `shouldBe` Right [
          (UInt256Type, "name"),
          (StringType, "oldname")
          ]
    it "should leave correct state" $ do
      s `shouldBe` ""
  
parseFunctionModifiers :: Spec
parseFunctionModifiers = do
  describe "parse function modifiers" $ do
    let str = "public  view"
    let (result, s) = runParser pFunctionModifiers str
    it "could parse the args" $ do
      result `shouldBe` Right ["public","view"]
    it "should leave correct state" $ do
      s `shouldBe` ""
  describe "parse function modifiers with curly bracket" $ do
    let str = "public  view {"
    let (result, s) = runParser pFunctionModifiers str
    it "could parse the args" $ do
      result `shouldBe` Right ["public","view"]
    it "should leave correct state" $ do
      s `shouldBe` "{"  
  describe "parse function modifiers" $ do
    let str = "public {"
    let (result, s) = runParser pFunctionModifiers str
    it "could parse the args" $ do
      result `shouldBe` Right ["public"]
    it "should leave correct state" $ do
      s `shouldBe` "{"
  describe "parse function modifiers with curly bracket and returns" $ do
    let str = "public  view returns {"
    let (result, s) = runParser pFunctionModifiers str
    it "could parse the args" $ do
      result `shouldBe` Right ["public","view"]
    it "should leave correct state" $ do
      s `shouldBe` "returns {" 

parseReturnsClosureSpec :: Spec
parseReturnsClosureSpec = do
  describe "parse function modifiers with curly bracket and returns" $ do
    let str = "returns (uint256){"
    let (result, s) = runParser pReturnsClosure str
    it "could parse the args" $ do
      result `shouldBe` Right UInt256Type
    it "should leave correct state" $ do
      s `shouldBe` "{" 


parseCommentSpec :: Spec
parseCommentSpec = do
  describe "parse comment" $ do
    let comment = "//helloworld _*&^"
    let (result, s) = runParser pComment comment
    it "get the correct comment string" $ do
      result `shouldBe` Right "helloworld _*&^"
    it "left the correct state" $ do
      s `shouldBe` ""

  describe "parse comment with newline" $ do
    let comment = "//helloworld _*&^\n"
    let (result, s) = runParser pComment comment
    it "get the correct comment string" $ do
      result `shouldBe` Right "helloworld _*&^"
    it "left the correct state" $ do
      s `shouldBe` ""
  describe "parse comment with newline and many leading spaces" $ do
    let comment = "//   helloworld _*&^\n"
    let (result, s) = runParser pComment comment
    it "get the correct comment string" $ do
      result `shouldBe` Right "   helloworld _*&^"
    it "left the correct state" $ do
      s `shouldBe` ""

parseSPDXCommentSpec :: Spec
parseSPDXCommentSpec = do
  describe "parse SPDX comment" $ do
    let comment = "// SPDX-License-Identifier: MIT"
    let (result, s) = runParser pSPDXComment comment
    it "get the correct comment string" $ do
      result `shouldBe` Right "MIT"
    it "left the correct state" $ do
      s `shouldBe` ""
  describe "parse SPDX comment" $ do
    let comment = "// SPDX-License-Identifier: BSD-2"
    let (result, s) = runParser pSPDXComment comment
    it "get the correct comment string" $ do
      result `shouldBe` Right "BSD-2"
    it "left the correct state" $ do
      s `shouldBe` ""

parsePragmaSpec :: Spec
parsePragmaSpec = do
  describe "parse pragma version ^0.8.24" $ do
    let input = "pragma solidity ^0.8.24;"
    let (result, s) = runParser pPragma input
    it "get the correct comment string" $ do
      result
        `shouldBe` Right
          ( SemVer {major = 0, minor = 8, patch = Just 24, semVerRangeMark = Just Caret}
          )
    it "left the correct state" $ do
      s `shouldBe` ""
  describe "parse pragma version ~0.8.24" $ do
    let input = "pragma solidity ~0.8.24;"
    let (result, s) = runParser pPragma input
    it "get the correct comment string" $ do
      result
        `shouldBe` Right
          ( SemVer {major = 0, minor = 8, patch = Just 24, semVerRangeMark = Just Tilde}
          )
    it "left the correct state" $ do
      s `shouldBe` ""
  describe "parse pragma version *" $ do
    let input = "pragma solidity *;"
    let (result, s) = runParser pPragma input
    it "get the correct comment string" $ do
      result
        `shouldBe` Right
          ( SemVer {major = 0, minor = 0, patch = Nothing, semVerRangeMark = Just Wildcards}
          )
    it "left the correct state" $ do
      s `shouldBe` ""
  describe "parse pragma version 0.8.24" $ do
    let input = "pragma solidity 0.8.24;"
    let (result, s) = runParser pPragma input
    it "get the correct comment string" $ do
      result
        `shouldBe` Right
          ( SemVer {major = 0, minor = 8, patch = Just 24, semVerRangeMark = Nothing}
          )
    it "left the correct state" $ do
      s `shouldBe` ""
