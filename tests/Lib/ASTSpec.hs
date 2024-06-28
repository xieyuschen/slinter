{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Lib.ASTSpec (spec) where

-- export multiple test functions is not supported, as the document reads:
-- Each spec file has to export a top-level binding spec of type Spec.

import Control.Monad.Except (runExceptT)
import Control.Monad.State (runState)
import Lib.AST
import Lib.Parser
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    parseCommentSpec
    parseSPDXCommentSpec
    parsePragmaSpec

parseCommentSpec :: Spec
parseCommentSpec = do
  describe "parse comment" $ do
    let comment = "//helloworld _*&^"
    let (result, s) = runState (runExceptT pComment) comment
    it "get the correct comment string" $ do
      result `shouldBe` Right (ASTComment "helloworld _*&^")
    it "left the correct state" $ do
      s `shouldBe` ""

  describe "parse comment with newline" $ do
    let comment = "//helloworld _*&^\n"
    let (result, s) = runState (runExceptT pComment) comment
    it "get the correct comment string" $ do
      result `shouldBe` Right (ASTComment "helloworld _*&^")
    it "left the correct state" $ do
      s `shouldBe` ""
  describe "parse comment with newline and many leading spaces" $ do
    let comment = "//   helloworld _*&^\n"
    let (result, s) = runState (runExceptT pComment) comment
    it "get the correct comment string" $ do
      result `shouldBe` Right (ASTComment "   helloworld _*&^")
    it "left the correct state" $ do
      s `shouldBe` ""

parseSPDXCommentSpec :: Spec
parseSPDXCommentSpec = do
  describe "parse SPDX comment" $ do
    let comment = "// SPDX-License-Identifier: MIT"
    let (result, s) = runState (runExceptT pSPDXComment) comment
    it "get the correct comment string" $ do
      result `shouldBe` Right (ASTSPDXComment "MIT")
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
          ( ASTPragma $
              SemVer {major = 0, minor = 8, patch = Just 24, semVerRangeMark = Just Caret}
          )
    it "left the correct state" $ do
      s `shouldBe` ""
  describe "parse pragma version ~0.8.24" $ do
    let input = "pragma solidity ~0.8.24;"
    let (result, s) = runParser pPragma input
    it "get the correct comment string" $ do
      result
        `shouldBe` Right
          ( ASTPragma $
              SemVer {major = 0, minor = 8, patch = Just 24, semVerRangeMark = Just Tilde}
          )
    it "left the correct state" $ do
      s `shouldBe` ""
  describe "parse pragma version *" $ do
    let input = "pragma solidity *;"
    let (result, s) = runParser pPragma input
    it "get the correct comment string" $ do
      result
        `shouldBe` Right
          ( ASTPragma $
              SemVer {major = 0, minor = 0, patch = Nothing, semVerRangeMark = Just Wildcards}
          )
    it "left the correct state" $ do
      s `shouldBe` ""
  describe "parse pragma version 0.8.24" $ do
    let input = "pragma solidity 0.8.24;"
    let (result, s) = runParser pPragma input
    it "get the correct comment string" $ do
      result
        `shouldBe` Right
          ( ASTPragma $
              SemVer {major = 0, minor = 8, patch = Just 24, semVerRangeMark = Nothing}
          )
    it "left the correct state" $ do
      s `shouldBe` ""
