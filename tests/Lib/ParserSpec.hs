module Lib.ParserSpec (spec) where

-- export multiple test functions is not supported, as the document reads:
-- Each spec file has to export a top-level binding spec of type Spec.

import Control.Monad (forM_)
import Data.Text (Text)
import qualified Data.Text as T
import Lib.Parser
  ( Parser,
    SemVer (SemVer, major, minor, patch, semVerRangeMark),
    SemVerRangeMark (Tilde, Wildcards),
    pIdentifier,
    pManySpaces,
    pOneKeyword,
    pSemVer,
    runParser,
  )
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  parseManyTokensSpec
  parseVersionSpec

structCom :: Parser (Text, Text, Text)
structCom = do
  k1 <- pOneKeyword "type" <* pManySpaces
  ident <- pIdentifier <* pManySpaces
  k2 <-
    pOneKeyword "struct"
      <* ( pManySpaces
             >> pOneKeyword "{"
             >> pManySpaces
             >> pOneKeyword "}"
         )

  return (k1, ident, k2)

parseManyTokensSpec :: Spec
parseManyTokensSpec = do
  let succeededCases =
        [ ( "type Helloworld struct {  } _+",
            ("type", "Helloworld", "struct"),
            " _+"
          ),
          ( "type Hello_world struct {  } _+",
            ("type", "Hello_world", "struct"),
            " _+"
          ),
          ( "type Hello123world struct {  } _+",
            ("type", "Hello123world", "struct"),
            " _+"
          ),
          ( "type Helloworld struct {  } _+",
            ("type", "Helloworld", "struct"),
            " _+"
          ),
          ( "type _Helloworld struct {  } _+",
            ("type", "_Helloworld", "struct"),
            " _+"
          )
        ]
  forM_ succeededCases $ \(input, expected, left) ->
    describe "parse helloworld identifier" $ do
      let (result, s') = runParser structCom input
      it "could parse the result successfully" $ do
        result `shouldBe` Right expected
      it "leave the correct state" $ do
        s' `shouldBe` left

parseVersionSpec :: Spec
parseVersionSpec = do
  let testCases =
        [ ("~0.8.24", Right (SemVer {major = 0, minor = 8, patch = Just 24, semVerRangeMark = Just Tilde}), ""),
          ("*", Right (SemVer {major = 0, minor = 0, patch = Nothing, semVerRangeMark = Just Wildcards}), ""),
          ("0.8.24", Right (SemVer {major = 0, minor = 8, patch = Just 24, semVerRangeMark = Nothing}), "")
        ]
  forM_ testCases f
  where
    f (input, expectedResult, expectedState) =
      describe ("parse version " ++ input) $ do
        let (result, s) = runParser pSemVer $ T.pack input
        it "gets the correct comment string" $ do
          result `shouldBe` expectedResult
        it "leaves the correct state" $ do
          s `shouldBe` expectedState
