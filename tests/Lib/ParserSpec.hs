{-# LANGUAGE OverloadedStrings #-}

module Lib.ParserSpec (spec) where

-- export multiple test functions is not supported, as the document reads:
-- Each spec file has to export a top-level binding spec of type Spec.

import Control.Monad (forM_)
import Lib.Parser
  ( SemVer (SemVer, major, minor, patch, semVerRangeMark),
    SemVerRangeMark (Tilde, Wildcards),
    pSemVer,
    pString,
  )
import Lib.TestCommon (verifyParser)
import Test.Hspec (Spec)

spec :: Spec
spec = do
  parseVersionSpec
  parseStringSpec

parseVersionSpec :: Spec
parseVersionSpec = do
  let testCases =
        [ ("~0.8.24", Right (SemVer {major = 0, minor = 8, patch = Just 24, semVerRangeMark = Just Tilde}), ""),
          ("*", Right (SemVer {major = 0, minor = 0, patch = Nothing, semVerRangeMark = Just Wildcards}), ""),
          ("0.8.24", Right (SemVer {major = 0, minor = 8, patch = Just 24, semVerRangeMark = Nothing}), "")
        ]
  forM_ testCases $ verifyParser "version" pSemVer

parseStringSpec :: Spec
parseStringSpec = do
  let testCases =
        [ ("\"hello\"", Right "hello", ""),
          ("\"./MyLibrary.sol\"", Right "./MyLibrary.sol", ""),
          ("unicode\"Hello 😃\"", Right "Hello 😃", ""),
          -- hex should be converted to the string correctly
          ("hex\"4142434445\"", Right "ABCDE", "")
        ]
  forM_ testCases $ verifyParser "parse string" pString
