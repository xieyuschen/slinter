{-# LANGUAGE OverloadedStrings #-}
module Lib.LexierSpec(spec) where
-- export multiple test functions is not supported, as the document reads:
-- Each spec file has to export a top-level binding spec of type Spec.

import Test.Hspec ( Spec, describe, it, shouldBe )
import Lib.Lexier
import Control.Monad.State (runState)
import Control.Monad.Except (runExceptT)

spec :: Spec
spec = do
  let input = "type Helloworld struct {  } "
  describe "parse many" $ do
    let structCom = (pure <$> parseOne "type" Keyword) 
                    `parseThen` parseMany parseSpace
                    `parseThen` (pure <$> parseIdentifier)
                    `parseThen` parseMany parseSpace
                    `parseThen` (pure <$> parseOne "struct" Keyword)
                    `parseThen` parseMany parseSpace
                    `parseThen` (pure <$> parseOne "{" Operator)
                    `parseThen` parseMany parseSpace
                    `parseThen` (pure <$> parseOne "}" Operator)
    let (result, s') = runState (runExceptT structCom) input
    it "should parse correctly" $ do
      result `shouldBe` Right [Keyword "type"
                                , Whitespace
                                , Identifier "Helloworld"
                                , Whitespace
                                , Keyword "struct"
                                , Whitespace
                                , Operator "{"
                                , Whitespace
                                , Whitespace
                                , Operator "}"] 
    it "should have correct state" $ do
      s' `shouldBe` " "