module Lib.LexierSpec(spec) where
-- export multiple test functions is not supported, as the document reads:
-- Each spec file has to export a top-level binding spec of type Spec.

import Test.Hspec
import Lib.Lexier

spec :: Spec
spec = do
  describe "scanOneWord with valid string" $ do
    let input = "foo = 42 + bar"
    let (re, s) = executeScaner scanOneWord input
    it "should scan correctly" $ do
      re `shouldBe` ["foo"]
    it "should have the correct new state" $ do
      s `shouldBe` "= 42 + bar"

  describe "scanOneWord with empty string" $ do
    let input = ""
    let (re, s) = executeScaner scanOneWord input
    it "should scan correctly" $ do
      re `shouldBe` [""]
    it "should have the correct new state" $ do
      s `shouldBe` ""

  describe "scanMultipleWords with valid string" $ do
    let input = "foo = 42 + bar"
    let (re, s) = executeScaner (scanMultipleWords 1) input
    it "should scan correctly" $ do
      re `shouldBe` ["foo"]
    it "should have the correct new state" $ do
      s `shouldBe` "= 42 + bar"
  describe "scanMultipleWords with valid string" $ do
    let input = "foo = 42 + bar"
    let (re, s) = executeScaner (scanMultipleWords 5) input
    it "should scan correctly" $ do
      re `shouldBe` ["foo","=", "42", "+", "bar"]
    it "should have the correct new state" $ do
      s `shouldBe` ""
  describe "scanMultipleWords with valid string" $ do
    let input = "foo = 42 + bar"
    let (re, s) = executeScaner (scanMultipleWords 6) input
    it "should scan correctly" $ do
      re `shouldBe` ["foo","=", "42", "+", "bar",""]
    it "should have the correct new state" $ do
      s `shouldBe` ""
  describe "scanMultipleWords with empty string" $ do
    let input = ""
    let (re, s) = executeScaner (scanMultipleWords 1) input
    it "should scan correctly" $ do
      re `shouldBe` [""]
    it "should have the correct new state" $ do
      s `shouldBe` ""
    
  describe "scanAllWords with valid string" $ do
    let input = "foo = 42 + bar"
    let (re, s) = executeScaner scanAllWords input
    it "should scan correctly" $ do
      re `shouldBe` ["foo","=", "42", "+", "bar"]
    it "should have the correct new state" $ do
      s `shouldBe` ""