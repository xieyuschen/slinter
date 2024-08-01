{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.TestCommon where

import Data.Bifunctor (first)
import Data.Either
import Data.Text (Text)
import qualified Data.Text as T
import Lib.Parser
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)
import Text.Parsec.Error
import Text.Parsec.Pos

resultIsRight :: Either a Integer
resultIsRight = Right (0 :: Integer)

-- Verifier is used to check the parser result
type Verifier a b = Text -> Parser a -> (Text, Either [Text] b, Text) -> Spec

-- Justifier is used by a verifier to check the parser result is expected or not
type Justifier a b = Either [Text] b -> ParserResult a -> Spec

newParserVerifier :: Justifier a b -> Verifier a b
newParserVerifier j content parser (input, expectedResult, expectedState) = do
  describe (T.unpack $ "parse " <> content <> ": '" <> input <> "'") $ do
    let (result, s) = runSParser parser input
    j expectedResult result
    it "should leave correct state" $ do
      s `shouldBe` expectedState

-- leftRightJustifier only checks whether the result is left or right,
-- it won't respect the value of right, but it will validate the error position for the right
leftRightJustifier :: Justifier a b
leftRightJustifier expectedResult result = do
  it "parse result should be correct" $ do
    case expectedResult of
      Right _ ->
        if isRight result
          then return ()
          else expectationFailure $ "Expected Right but got Left: " ++ show (fromLeft (error "") result)
      Left _ -> do
        errorPos (fromLeft (error "") result) `shouldBe` initialPos ""

exactlyJustifier :: (Eq a, Show a) => Justifier a a
exactlyJustifier expectedResult result = do
  it "could parse the content correctly" $ do
    case expectedResult of
      Right r -> result `shouldBe` Right r
      Left _ -> do
        first
          ( fmap T.pack
              . filter (not . null)
              . fmap messageString
              . errorMessages
          )
          result
          `shouldBe` expectedResult

exactlyParserVerifier :: (Eq a, Show a) => Verifier a a
exactlyParserVerifier = newParserVerifier exactlyJustifier

appendSuffix :: Text -> (Text, Either [Text] a, Text) -> (Text, Either [Text] a, Text)
appendSuffix s (input, expected, state) = (input <> s, expected, state <> s)
