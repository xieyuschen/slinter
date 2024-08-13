{-# LANGUAGE OverloadedStrings #-}

module Lib.AST.FileSpec (spec) where

import Data.Bifunctor (first)
import Data.Text (pack)
import Lib.AST.File (pWholeSolFile)
import Lib.AST.Model (SolFile (..))
import Lib.Parser (SemVer (..), SemVerRangeMark (..), runSParser)
import Test.Hspec (Spec, beforeAll, describe, it, shouldBe)
import Text.Parsec.Error (errorMessages, messageString)

-- import Text.Parsec.Pos
-- import Data.Either

spec :: Spec
spec = do
  parseFileSpec

-- todo: fix me: abstract me into testcommon
parseFileSpec :: Spec
parseFileSpec = beforeAll
  ( do
      content <- readFile "tests/Lib/testdata/test.sol"
      let parsed = runSParser pWholeSolFile $ pack content
      return parsed
  )
  $ describe "read and parse the sol file correctly"
  $ do
    it "parses with correct result" $ \(result, _) ->
      do
        first
          ( fmap pack
              . filter (not . null)
              . fmap messageString
              . errorMessages
          )
          result
        `shouldBe` Right
          ( SolFile
              { solFileName = "",
                solPragma = SemVer {major = 0, minor = 8, patch = Just 24, semVerRangeMark = Just Caret},
                solSpdxLicense = "MIT",
                solImprotDirectives = [],
                solUsingDirectives = [],
                solContracts = [],
                solInterfaces = [],
                solLibraries = [],
                solFunctions = [],
                solConstantVars = [],
                solStructs = [],
                solEnums = [],
                solUserDefineValueType = [],
                solErrors = [],
                solEvents = []
              }
          )

    it "parses with an empty remaining string" $ \(_, s) -> do
      s `shouldBe` ""
