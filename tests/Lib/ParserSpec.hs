{-# LANGUAGE OverloadedStrings #-}

module Lib.ParserSpec (spec) where

-- export multiple test functions is not supported, as the document reads:
-- Each spec file has to export a top-level binding spec of type Spec.

import Control.Monad.Except (runExceptT)
import Control.Monad.State (runState)
import Lib.Parser
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  parseManyTokensSpec
  parseVersionSpec

parseManyTokensSpec :: Spec
parseManyTokensSpec = do
  describe "parse many" $ do
    let input = "type Helloworld struct {  } _+"
    let structCom = do
          k1 <- pOne "type" id
          _ <- pMany pSpace
          ident <- pIdentifier
          _ <- parseManySpaces
          k2 <- pOneKeyword "struct"
          _ <- parseManySpaces
          _ <- pOne "{" id
          _ <- parseManySpaces
          _ <- pOne "}" id
          return (k1, ident, k2)
    let (result, s') = runState (runExceptT structCom) input
    it "should parse correctly" $ do
      result
        `shouldBe` Right
          ("type", "Helloworld", "struct")
    it "should have correct state" $ do
      s' `shouldBe` " _+"
      
parseVersionSpec :: Spec
parseVersionSpec = do
  describe "parse version ~0.8.24" $ do
    let input = "~0.8.24"
    let (result, s) = runParser pSemVer input
    it "get the correct comment string" $ do
      result
        `shouldBe` Right
          ( 
              SemVer {major = 0, minor = 8, patch = Just 24, semVerRangeMark = Just Tilde}
          )
    it "left the correct state" $ do
      s `shouldBe` ""
  describe "parse  version *" $ do
    let input = "*"
    let (result, s) = runParser pSemVer input
    it "get the correct comment string" $ do
      result
        `shouldBe` Right
          ( 
              SemVer {major = 0, minor = 0, patch = Nothing, semVerRangeMark = Just Wildcards}
          )
    it "left the correct state" $ do
      s `shouldBe` ""
  describe "parse version 0.8.24" $ do
    let input = "0.8.24"
    let (result, s) = runParser pSemVer input
    it "get the correct comment string" $ do
      result
        `shouldBe` Right
          ( 
              SemVer {major = 0, minor = 8, patch = Just 24, semVerRangeMark = Nothing}
          )
    it "left the correct state" $ do
      s `shouldBe` ""