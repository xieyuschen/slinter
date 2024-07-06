{-# LANGUAGE OverloadedStrings #-}

module Lib.ParserSpec (spec) where

-- export multiple test functions is not supported, as the document reads:
-- Each spec file has to export a top-level binding spec of type Spec.

import Control.Monad (forM_)
import Lib.Parser
  ( Parser,
    SemVer (SemVer, major, minor, patch, semVerRangeMark),
    SemVerRangeMark (Tilde, Wildcards),
    pIdentifier,
    pMany,
    pManySpaces,
    pOne,
    pOneKeyword,
    pSemVer,
    pSpace,
    runParser,
  )
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  parseManyTokensSpec
  parseVersionSpec

structCom :: Lib.Parser.Parser (String, String, String)
structCom = do
  k1 <- Lib.Parser.pOne "type" id
  _ <- Lib.Parser.pMany Lib.Parser.pSpace
  ident <- Lib.Parser.pIdentifier
  _ <- Lib.Parser.pManySpaces
  k2 <- Lib.Parser.pOneKeyword "struct"
  _ <- Lib.Parser.pManySpaces
  _ <- Lib.Parser.pOne "{" id
  _ <- Lib.Parser.pManySpaces
  _ <- Lib.Parser.pOne "}" id
  return (k1, ident, k2)

parseManyTokensSpec :: Spec
parseManyTokensSpec = do
  let succeededCases =
        [ ( "type Helloworld struct {  } _+",
            ("type", "Helloworld", "struct"),
            " _+"
          ),
          ( "type Hello_world struct {  } _+",
            ("type", "Hello_world", "struct"),
            " _+"
          ),
          ( "type Hello123world struct {  } _+",
            ("type", "Hello123world", "struct"),
            " _+"
          ),
          ( "type Helloworld struct {  } _+",
            ("type", "Helloworld", "struct"),
            " _+"
          ),
          ( "type _Helloworld struct {  } _+",
            ("type", "_Helloworld", "struct"),
            " _+"
          )
        ]
  forM_ succeededCases $ \(input, expected, left) ->
    describe "parse helloworld identifier" $ do
      let (result, s') = Lib.Parser.runParser structCom input
      it "could parse the result successfully" $ do
        result `shouldBe` Right expected
      it "leave the correct state" $ do
        s' `shouldBe` left

parseVersionSpec :: Spec
parseVersionSpec = do
  let testCases =
        [ ("~0.8.24", Right (Lib.Parser.SemVer {major = 0, minor = 8, patch = Just 24, semVerRangeMark = Just Lib.Parser.Tilde}), ""),
          ("*", Right (Lib.Parser.SemVer {major = 0, minor = 0, patch = Nothing, semVerRangeMark = Just Lib.Parser.Wildcards}), ""),
          ("0.8.24", Right (Lib.Parser.SemVer {major = 0, minor = 8, patch = Just 24, semVerRangeMark = Nothing}), "")
        ]
  forM_ testCases f
  where
    f (input, expectedResult, expectedState) =
      describe ("parse version " ++ input) $ do
        let (result, s) = Lib.Parser.runParser Lib.Parser.pSemVer input
        it "gets the correct comment string" $ do
          result `shouldBe` expectedResult
        it "leaves the correct state" $ do
          s `shouldBe` expectedState
