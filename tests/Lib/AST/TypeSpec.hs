{-# LANGUAGE OverloadedStrings #-}

module Lib.AST.TypeSpec where

import Control.Monad (forM_)
import Lib.AST.Model
import Lib.AST.Type
import Lib.Parser
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  parseTypeEnumSpec
  parseTypeAliasSpec
  parseTypeStructureSpec
  parseTypeDifinitionSpec
  parseTypeWithBitLengthSpec
  let testCases =
        [ ( "uint",
            Right $ STypeUint 256,
            ""
          ),
          ( "uint8",
            Right $ STypeUint 8,
            ""
          ),
          ( "uint7",
            Left "",
            ""
          ),
          ( "int128",
            Right $ STypeInt 128,
            ""
          ),
          ( "int257",
            Left "",
            ""
          ),
          ( "custom8",
            Right $ STypeCustom "custom8",
            ""
          ),
          ( "custom7",
            Right $ STypeCustom "custom7",
            ""
          ),
          ( "custom7abc_",
            Right $ STypeCustom "custom7abc_",
            ""
          ),
          ( "fixed",
            Right $ STypeFixed 128 18,
            ""
          ),
          ( "fixed128x28",
            Right $ STypeFixed 128 28,
            ""
          ),
          ( "ufixed256x35",
            Right $ STypeUFixed 256 35,
            ""
          ),
          ( "fixed256",
            Right $ STypeCustom "fixed256",
            ""
          ),
          ( "fixed257x100",
            Left "",
            ""
          ),
          ( "address",
            Right STypeAddress,
            ""
          ),
          ( "address payable",
            Right STypePayableAddress,
            ""
          ),
          ( "bytes",
            Right $ STypeBytes 1,
            ""
          ),
          ( "bytes2",
            Right $ STypeBytes 2,
            ""
          ),
          ( "bytes9",
            Left "",
            ""
          ),
          ( "mapping(address => uint256) private _balances;",
            Right $
              STypeMapping $
                Mapping
                  { mKeyType = STypeAddress,
                    mValueType = STypeUint 256
                  },
            "private _balances;"
          ),
          ( "mapping(address => mapping(address => uint256)) private _allowances;",
            Right $
              STypeMapping $
                Mapping
                  { mKeyType = STypeAddress,
                    mValueType =
                      STypeMapping $
                        Mapping
                          { mKeyType = STypeAddress,
                            mValueType = STypeUint 256
                          }
                  },
            "private _allowances;"
          )
        ]
  forM_ testCases $ \(input, expected, left) ->
    describe ("parse type: " ++ input) $ do
      let (result, s) = runParser pType input
      it "gets the correct type" $ do
        result `shouldBe` expected
      it "leaves the correct state" $ do
        s `shouldBe` left

parseTypeWithBitLengthSpec :: Spec
parseTypeWithBitLengthSpec = do
  let testCases =
        [ ("uint", ("uint", Nothing), ""),
          ("uint8", ("uint", Just $ BitLength 8), ""),
          ("uint128", ("uint", Just $ BitLength 128), ""),
          ("int", ("int", Nothing), ""),
          ("int16", ("int", Just $ BitLength 16), ""),
          ("custom256", ("custom256", Nothing), ""),
          ("custom256type", ("custom256type", Nothing), ""),
          ("custom256x18", ("custom256x18", Nothing), ""),
          ("fixed256", ("fixed256", Nothing), ""),
          ("fixed", ("fixed", Nothing), ""),
          ("fixed256x80", ("fixed", Just $ BitLengthWithDecimal 256 80), ""),
          ("bytes", ("bytes", Nothing), ""),
          ("bytes3", ("bytes", Just $ BitLength 3), ""),
          ("bytes9", ("bytes", Just $ BitLength 9), "")
        ]
  forM_ testCases $ \(input, expected, left) ->
    describe "parse the type correctly" $ do
      let (result, s') = runParser pTypeWithBitLength input
      it "could parse the result successfully" $ do
        result `shouldBe` Right expected
      it "leave the correct state" $ do
        s' `shouldBe` left

parseTypeEnumSpec :: Spec
parseTypeEnumSpec = do
  let testCases =
        [ ( "enum _Ab { A1}",
            Right $
              STypeEnum
                { ename = "_Ab",
                  eelems = ["A1"]
                },
            ""
          ),
          ( "enum TEST { A1, a2, A3_, A_4 }",
            Right $
              STypeEnum
                { ename = "TEST",
                  eelems = ["A1", "a2", "A3_", "A_4"]
                },
            ""
          ),
          ( "enum TEST { } test state",
            Left "Failed to parse identifier",
            "} test state"
          )
        ]
  forM_ testCases $ \(input, expected, left) ->
    describe ("parse the input correctly: " ++ input) $ do
      let (result, s') = runParser pTypeEnum input
      it "could parse the result successfully" $ do
        result `shouldBe` expected
      it "leave the correct state" $ do
        s' `shouldBe` left

parseTypeAliasSpec :: Spec
parseTypeAliasSpec = do
  let testCases =
        [ ( "type Alias1 is uint256",
            Right $
              SAlias
                { salias = "Alias1",
                  saliasOriginType = STypeUint 256
                },
            ""
          ),
          ( "type _Ab is address",
            Right $
              SAlias
                { salias = "_Ab",
                  saliasOriginType = STypeAddress
                },
            ""
          ),
          ( "type is uint256",
            Left "",
            "uint256"
          )
        ]
  forM_ testCases $ \(input, expected, left) ->
    describe ("parse the input correctly: " ++ input) $ do
      let (result, s') = runParser pTypeAlias input
      it "could parse the result successfully" $ do
        result `shouldBe` expected
      it "leave the correct state" $ do
        s' `shouldBe` left

parseTypeStructureSpec :: Spec
parseTypeStructureSpec = do
  let whitespace = " "
      testCases =
        [ ( "struct empty {} ",
            Right $
              Structure
                { structName = "empty",
                  structFields = []
                },
            whitespace
          ),
          ( "struct empty { uint128 price; } ",
            Right $
              Structure
                { structName = "empty",
                  structFields = [(STypeUint 128, "price")]
                },
            whitespace
          ),
          ( "struct empty { \n uint128 price; } ",
            Right $
              Structure
                { structName = "empty",
                  structFields = [(STypeUint 128, "price")]
                },
            whitespace
          ),
          -- todo: fix me as this should be error
          ( "struct empty { \n uint128 \n price; } ",
            Right $
              Structure
                { structName = "empty",
                  structFields = [(STypeUint 128, "price")]
                },
            whitespace
          ),
          ( "struct empty { \n uint128 price; address addr;} ",
            Right $
              Structure
                { structName = "empty",
                  structFields = [(STypeUint 128, "price"), (STypeAddress, "addr")]
                },
            whitespace
          ),
          ( "struct empty { \n uint128 price; \n address addr; \n } ",
            Right $
              Structure
                { structName = "empty",
                  structFields = [(STypeUint 128, "price"), (STypeAddress, "addr")]
                },
            whitespace
          ),
          ( "struct empty { \n uint128 price; address;}",
            Left "",
            ";}"
          ),
          ( "struct {} test state",
            Left "Failed to parse identifier",
            "{} test state"
          )
        ]
  forM_ testCases $ \(input, expected, left) ->
    describe ("parse the input correctly: " ++ input) $ do
      let (result, s') = runParser pTypeStruct input
      it "could parse the result successfully" $ do
        result `shouldBe` expected
      it "leave the correct state" $ do
        s' `shouldBe` left

parseTypeDifinitionSpec :: Spec
parseTypeDifinitionSpec = do
  let whitespace = " "
      testCases =
        [ ( "struct empty {} ",
            Right $
              STypeStructure $
                Structure
                  { structName = "empty",
                    structFields = []
                  },
            whitespace
          ),
          ( "type Alias1 is uint256",
            Right $
              STypeAlias $
                SAlias
                  { salias = "Alias1",
                    saliasOriginType = STypeUint 256
                  },
            ""
          )
        ]
  forM_ testCases $ \(input, expected, left) ->
    describe ("parse the input correctly: " ++ input) $ do
      let (result, s') = runParser pTypeDefinition input
      it "could parse the result successfully" $ do
        result `shouldBe` expected
      it "leave the correct state" $ do
        s' `shouldBe` left
