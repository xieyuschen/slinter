{-# LANGUAGE OverloadedStrings #-}

module Lib.AST.ContractSpec (spec) where

import Lib.AST.Contract (pContract)
import Lib.AST.Function (getCtFunction)
import Lib.AST.Model
import Lib.Parser (runSParser)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  parseContractSpec

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
    let (result, s) = runSParser pContract constractStr
    it "get the correct contract" $ do
      let fns =
            [ Function
                { fname = FnNormal "get",
                  fnVisibility = FnPublic,
                  fnState = FnStateView,
                  fnIsVirtual = False,
                  fnModifierInvocations = [],
                  fnFnOverrideSpecifier = Nothing,
                  fargs = [],
                  fnReturnTyp = Just $ STypeUint 256,
                  fnBody = Just [StatReturn (Just (SExprVar "count"))]
                }
            ]
      let vars =
            [ StateVariable
                { svVisibleSpecifier = FnPublic,
                  svType = STypeUint 256,
                  svName = "count",
                  svComment = Just " Function to get the current count ",
                  svVarExpr = Nothing
                }
            ]
      result
        `shouldBe` Right
          Contract
            { ctName = "Counter",
              ctFunctions = fns,
              ctVariables = vars
            }
    it "get the correct state" $ do
      s `shouldBe` ""

isCtSpec :: Spec
isCtSpec = do
  describe "test IsCt function" $ do
    let f =
          Function
            { fnReturnTyp = Just $ STypeUint 256,
              fnVisibility = FnPublic,
              fnState = FnStateDefault,
              fnIsVirtual = False,
              fnModifierInvocations = [],
              fnFnOverrideSpecifier = Nothing,
              fargs = [],
              fname = FnNormal "inc",
              fnBody = Just [StatReturn (Just (SExprVar "count"))]
            }
    it "should return just function" $ do
      getCtFunction (CtFunction f) `shouldBe` Just f
    it "should return nothing because it's a varaible" $ do
      getCtFunction
        ( CtVariable
            StateVariable
              { svVisibleSpecifier = FnPublic,
                svType = STypeBool,
                svName = "",
                svComment = Nothing, -- attached comment
                svVarExpr = Nothing
              }
        )
        `shouldBe` Nothing
