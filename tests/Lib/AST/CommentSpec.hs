module Lib.AST.CommentSpec (spec) where

import Control.Monad (forM_)
import Lib.AST.Comment
import Lib.Parser
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  parseCommentSpec
  parseSPDXCommentSpec
  parsePragmaSpec

parseCommentSpec :: Spec
parseCommentSpec = do
  let testCases =
        [ ( "//helloworld _*&^",
            Right "helloworld _*&^",
            ""
          ),
          ( "//helloworld _*&^\n",
            Right "helloworld _*&^",
            ""
          ),
          ( "//   helloworld _*&^\n",
            Right "   helloworld _*&^",
            ""
          )
        ]
  forM_ testCases $ \(input, expectedResult, expectedState) -> do
    describe ("parse comment: " ++ input) $ do
      let (result, s) = runParser pComment input
      it "gets the correct comment string" $ do
        result `shouldBe` expectedResult
      it "leaves the correct state" $ do
        s `shouldBe` expectedState

parseSPDXCommentSpec :: Spec
parseSPDXCommentSpec = do
  let testCases =
        [ ( "// SPDX-License-Identifier: MIT",
            Right "MIT",
            ""
          ),
          ( "// SPDX-License-Identifier: BSD-2",
            Right "BSD-2",
            ""
          )
        ]
  forM_ testCases $ \(input, expectedResult, expectedState) -> do
    describe ("parse SPDX comment: " ++ input) $ do
      let (result, s) = runParser pSPDXComment input
      it "gets the correct comment string" $ do
        result `shouldBe` expectedResult
      it "leaves the correct state" $ do
        s `shouldBe` expectedState

parsePragmaSpec :: Spec
parsePragmaSpec = do
  let testCases =
        [ ( "pragma solidity ^0.8.24;",
            Right (SemVer {major = 0, minor = 8, patch = Just 24, semVerRangeMark = Just Caret}),
            ""
          ),
          ( "pragma solidity ~0.8.24;",
            Right (SemVer {major = 0, minor = 8, patch = Just 24, semVerRangeMark = Just Tilde}),
            ""
          ),
          ( "pragma solidity *;",
            Right (SemVer {major = 0, minor = 0, patch = Nothing, semVerRangeMark = Just Wildcards}),
            ""
          ),
          ( "pragma solidity 0.8.24;",
            Right (SemVer {major = 0, minor = 8, patch = Just 24, semVerRangeMark = Nothing}),
            ""
          )
        ]
  forM_ testCases $ \(input, expectedResult, expectedState) -> do
    describe ("parse pragma version: " ++ input) $ do
      let (result, s) = runParser pPragma input
      it "gets the correct pragma string" $ do
        result `shouldBe` expectedResult
      it "leaves the correct state" $ do
        s `shouldBe` expectedState
