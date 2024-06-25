module Lib.LexierSpec(spec) where
-- export multiple test functions is not supported, as the document reads:
-- Each spec file has to export a top-level binding spec of type Spec.

import Test.Hspec ( Spec, describe, it, shouldBe )
import Lib.Lexier
import Control.Monad.State ( runState )
import Data.Char (isDigit, isSpace)

spec :: Spec
spec = do
  let extracters = [(isString, consumeString), (isSpace, consumeSpace), (isDigit, consumeNum)]
  describe "consume one number" $ do
    let input = "12 34 56"
    let (tokens, s) = runState (scanOne extracters) input
    it "should scan correctly" $ do
      tokens `shouldBe` [Number 12]
    it "should have the correct new state" $ do
      s `shouldBe` " 34 56"

  describe "consume one space" $ do
    let input = " 34 56"
    let (tokens, s) = runState (scanOne extracters) input
    it "should scan correctly" $ do
      tokens `shouldBe` [Whitespace]
    it "should have the correct new state" $ do
      s `shouldBe` "34 56"

  describe "consume one word" $ do
    let input = "\"helloworld\" 123456"
    let (tokens, s) = runState (scanOne extracters) input
    it "should scan correctly" $ do
      tokens `shouldBe` [Word "\"helloworld\"" ]
    it "should have the correct new state" $ do
      s `shouldBe` " 123456"

  describe "consume multiple 1 tokens" $ do
    let input = "\"helloworld\" 123456"
    let (tokens, s) = runState (scanMultipleSt extracters 1) input
    it "should scan correctly" $ do
      tokens `shouldBe` [Word "\"helloworld\"" ]
    it "should have the correct new state" $ do
      s `shouldBe` " 123456"

  describe "consume multiple 3 tokens" $ do
    let input = "\"helloworld\" 123456"
    let (tokens, s) = runState (scanMultipleSt extracters 3) input
    it "should scan correctly" $ do
      tokens `shouldBe` [Word "\"helloworld\"", Whitespace, Number 123456]
    it "should have the correct new state" $ do
      s `shouldBe` ""

  describe "consume scanAll" $ do
    let input = "\"helloworld\" 123456"

    let (tokens, s) = runState (scanAllSt extracters) input
    it "should scan correctly" $ do
      tokens `shouldBe` [Word "\"helloworld\"", Whitespace, Number 123456]
    it "should have the correct new state" $ do
      s `shouldBe` ""