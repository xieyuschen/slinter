{-# LANGUAGE OverloadedStrings #-}

module Lib.AST.ContractSpec (spec) where

import Control.Monad (forM_)
import Lib.AST.Contract
import Lib.AST.Function
import Lib.AST.Model
import Lib.Parser
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  parseContractSpec
  parseContractVariableSpec

parseContractSpec :: Spec
parseContractSpec = do
  isCtSpec
  describe "parse simple StateVariable" $ do
    let constractStr =
          "contract Counter { \
          \   uint256 public count;\
          \   // Function to get the current count \n \
          \   function get() public view returns (uint256) {\
          \       return count;\
          \    } \
          \}"
    let (result, s) = runParser pContract constractStr
    it "get the correct contract" $ do
      let fns =
            [ Function
                { fmodifiers = ["view"],
                  fname = "get",
                  fVisiblitySpecifier = VsPublic,
                  fargs = [],
                  fReturnTyp = Just $ STypeUint 256
                }
            ]
      let vars =
            [ StateVariable
                { svVisibleSpecifier = VsPublic,
                  svType = STypeUint 256,
                  svName = "count",
                  svComment = Just " Function to get the current count "
                }
            ]
      result
        `shouldBe` Right
          Contract
            { cname = "Counter",
              cfunctions = fns,
              cvariables = vars
            }
    it "get the correct state" $ do
      s `shouldBe` ""

parseContractVariableSpec :: Spec
parseContractVariableSpec = do
  let testCases =
        [ ( "uint256 public name;",
            Right
              StateVariable
                { svVisibleSpecifier = VsPublic,
                  svType = STypeUint 256,
                  svName = "name",
                  svComment = Nothing
                },
            ""
          ),
          ( "uint256 public name; // name refers to the contract name",
            Right
              StateVariable
                { svVisibleSpecifier = VsPublic,
                  svType = STypeUint 256,
                  svName = "name",
                  svComment = Just " name refers to the contract name"
                },
            ""
          )
        ]
  forM_ testCases $ \(input, expectedResult, expectedState) -> do
    describe ("parse variable: " ++ input) $ do
      let (result, s) = runParser pStateVariable input
      it "gets the correct variable" $ do
        result `shouldBe` expectedResult
      it "leaves the correct state" $ do
        s `shouldBe` expectedState

isCtSpec :: Spec
isCtSpec = do
  describe "test IsCt function" $ do
    let f =
          Function
            { fReturnTyp = Just $ STypeUint 256,
              fVisiblitySpecifier = VsPublic,
              fargs = [],
              fmodifiers = ["public"],
              fname = "inc"
            }
    it "should return just function" $ do
      getCtFunction (CtFunction f) `shouldBe` Just f
    it "should return nothing because it's a varaible" $ do
      getCtFunction (CtVariable StateVariable {}) `shouldBe` Nothing
