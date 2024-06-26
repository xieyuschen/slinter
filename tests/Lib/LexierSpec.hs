module Lib.LexierSpec(spec) where
-- export multiple test functions is not supported, as the document reads:
-- Each spec file has to export a top-level binding spec of type Spec.

import Test.Hspec ( Spec, describe, it, shouldBe )
import Lib.Lexier
import Control.Applicative ((<|>))

spec :: Spec
spec = do
  let extracters = consumeWord <|> consumeSpace <|> consumeNum
  describe "consume one number" $ do
    let input = "12 34 56"
    let (tokens, s) = scanOneSt extracters input
    it "should scan correctly" $ do
      tokens `shouldBe` Right [Number 12]
    it "should have the correct new state" $ do
      s `shouldBe` " 34 56"

  describe "consume one space" $ do
    let input = " 34 56"
    let (tokens, s) = scanOneSt extracters input
    it "should scan correctly" $ do
      tokens `shouldBe` Right [Whitespace]
    it "should have the correct new state" $ do
      s `shouldBe` "34 56"

  describe "consume one word" $ do
    let input = "\"helloworld\" 123456"
        (tokens, s) = scanOneSt extracters input
    it "should scan correctly" $ do
      tokens `shouldBe` Right [Word "\"helloworld\"" ]
    it "should have the correct new state" $ do
      s `shouldBe` " 123456"

  describe "consume one word and expect error" $ do
    let input = "\"helloworld 123456"
        (tokens, s) = scanOneSt extracters input
    it "should return left dur to lack right quote" $ do
      tokens `shouldBe` Left "missing right quote"
    it "should have the correct new state" $ do
      s `shouldBe` ""

  describe "consume multiple 1 tokens" $ do
    let input = "\"helloworld\" 123456"
    let (tokens, s) = scanMultipleSt extracters input 1
    it "should scan correctly" $ do
      tokens `shouldBe` Right [Word "\"helloworld\"" ]
    it "should have the correct new state" $ do
      s `shouldBe` " 123456"

  describe "consume multiple 3 tokens" $ do
    let input = "\"helloworld\" 123456"
    let (tokens, s) = scanMultipleSt extracters input 3
    it "should scan correctly" $ do
      tokens `shouldBe` Right [Word "\"helloworld\"", Whitespace, Number 123456]
    it "should have the correct new state" $ do
      s `shouldBe` ""

  describe "consume scanAll" $ do
    let input = "\"helloworld\" 123456"
    let tokens = scanAllSt extracters input
    it "should scan correctly" $ do
      tokens `shouldBe` Right [Word "\"helloworld\"", Whitespace, Number 123456]