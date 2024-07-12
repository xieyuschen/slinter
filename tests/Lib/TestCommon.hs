module Lib.TestCommon where

import Lib.Parser
import Test.Hspec (Spec, describe, it, shouldBe)

appendSuffix :: String -> (String, Either ErrMsg a, String) -> (String, Either ErrMsg a, String)
appendSuffix s (input, expected, state) = (input ++ s, expected, state ++ s)

verifyParser :: (Eq a, Show a) => String -> Parser a -> (String, Either ErrMsg a, String) -> Spec
verifyParser content parser (input, expectedResult, expectedState) = do
  describe ("parse " ++ content ++ ":" ++ input) $ do
    let (result, s) = runParser parser input
    it ("could parse the " ++ content) $ do
      result `shouldBe` expectedResult
    it "should leave correct state" $ do
      s `shouldBe` expectedState
