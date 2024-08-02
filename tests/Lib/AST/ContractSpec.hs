{-# LANGUAGE OverloadedStrings #-}

module Lib.AST.ContractSpec (spec) where

import Lib.AST.Contract
import Lib.TestCommon (leftRightJustifier, newParserVerifier, resultIsRight)
import Test.Hspec (Spec, describe)

spec :: Spec
spec = do
  parseContractSpec

-- because the parsed result is too long(3k characters),
-- and actually they are all tested in the respective parsers,
-- so we only test the right or left of the result instead of match the result with expectation exactly
parseContractSpec :: Spec
parseContractSpec = do
  describe "parse simple StateVariable" $ do
    let testCase =
          ( "contract Counter { \
            \   uint256 public count;\
            \   // function to get the current count \n \
            \   function get() public view returns (uint256) {\
            \       return count;\
            \    } \
            \}",
            resultIsRight,
            ""
          )
    newParserVerifier
      leftRightJustifier
      "parse contract correctly"
      pContractDefinition
      testCase
