{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.TestCommon where

import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as T
import Lib.Parser (Parser, runSParser)
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Parsec.Error

appendSuffix :: Text -> (Text, Either [Text] a, Text) -> (Text, Either [Text] a, Text)
appendSuffix s (input, expected, state) = (input <> s, expected, state <> s)

verifyParser :: (Eq a, Show a) => Text -> Parser a -> (Text, Either [Text] a, Text) -> Spec
verifyParser content parser (input, expectedResult, expectedState) = do
  describe (T.unpack $ "parse " <> content <> ": '" <> input <> "'") $ do
    let (result, s) = runSParser parser input
    -- let pos = newPos "sourceFile" 1 1
    it (T.unpack $ "could parse the " <> content) $ do
      case expectedResult of
        Right r -> result `shouldBe` Right r
        -- todo: fix the error cases later
        Left _ -> do
          -- we use this line to debug when it failed to parse
          -- the error is raised when the result it a Right instead of a Left
          -- errorPos (fromLeft (error "") result) `shouldBe` initialPos ""
          first
            ( fmap T.pack
                . filter (not . null)
                . fmap messageString
                . errorMessages
            )
            result
            `shouldBe` expectedResult
    -- Left msg -> Left (newErrorMessage (Expect msg) pos) `shouldBe` result
    it "should leave correct state" $ do
      s `shouldBe` expectedState
