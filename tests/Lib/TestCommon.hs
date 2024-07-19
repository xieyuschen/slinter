module Lib.TestCommon where

import Data.Text (Text)
import qualified Data.Text as T
import Lib.Parser (ErrMsg, Parser, runParser)
import Test.Hspec (Spec, describe, it, shouldBe)

appendSuffix :: Text -> (Text, Either ErrMsg a, Text) -> (Text, Either ErrMsg a, Text)
appendSuffix s (input, expected, state) = (input <> s, expected, state <> s)

verifyParser :: (Eq a, Show a) => Text -> Parser a -> (Text, Either ErrMsg a, Text) -> Spec
verifyParser content parser (input, expectedResult, expectedState) = do
  describe (T.unpack $ "parse " <> content <> ": " <> input) $ do
    let (result, s) = runParser parser input
    it (T.unpack $ "could parse the " <> content) $ do
      result `shouldBe` expectedResult
    it "should leave correct state" $ do
      s `shouldBe` expectedState
