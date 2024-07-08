{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Lib.ASTSpec (spec) where

-- export multiple test functions is not supported, as the document reads:
-- Each spec file has to export a top-level binding spec of type Spec.

import Control.Monad (forM_)
import Lib.AST
import Lib.Parser
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  parseFunctionAllSpec
  parsePragmaSpec
  parseContractAllSpec
  parseTypeAllSpec
  parseCommentsAndPragamaAllSpec

parseCommentsAndPragamaAllSpec :: Spec
parseCommentsAndPragamaAllSpec = do
  parseCommentSpec
  parseSPDXCommentSpec
  parsePragmaSpec

parseContractAllSpec :: Spec
parseContractAllSpec = do
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

parseFunctionAllSpec :: Spec
parseFunctionAllSpec = do
  parseFunctionQuotedArgs
  parseFunctionModifiers
  parseFunctionReturnsClauseSpec
  parseFunctionArgsSpec
  parseFunctionSignatureSpec

parseFunctionSignatureSpec :: Spec
parseFunctionSignatureSpec = do
  let testCases =
        [ ( "function inc(string name) public views { count += 1; }",
            Right
              Function
                { fReturnTyp = Nothing,
                  fargs = [(STypeString, "name")],
                  fVisiblitySpecifier = VsPublic,
                  fmodifiers = ["views"],
                  fname = "inc"
                },
            ""
          ),
          ( "function inc(string name, uint256 new_name) views private returns (uint256) { count += 1; }",
            Right
              Function
                { fReturnTyp = Just $ STypeUint 256,
                  fargs = [(STypeString, "name"), (STypeUint 256, "new_name")],
                  fVisiblitySpecifier = VsPrivate,
                  fmodifiers = ["views"],
                  fname = "inc"
                },
            ""
          ),
          ( "function inc() internal returns (uint256) { count += 1; } }",
            Right
              Function
                { fReturnTyp = Just $ STypeUint 256,
                  fargs = [],
                  fVisiblitySpecifier = VsInternal,
                  fmodifiers = [],
                  fname = "inc"
                },
            " }"
          ),
          ( "function inc() returns (uint256) { count += 1; } }",
            Left "visibility specifier should contain only one for each function",
            "returns (uint256) { count += 1; } }"
          )
        ]
  forM_ testCases $ \(input, expectedResult, expectedState) -> do
    describe ("parse function: " ++ input) $ do
      let (result, s) = runParser pFunction input
      it "gets the correct function" $ do
        result `shouldBe` expectedResult
      it "leaves the correct state" $ do
        s `shouldBe` expectedState

parseFunctionArgsSpec :: Spec
parseFunctionArgsSpec = do
  describe "parse empty arg with quotes" $ do
    let str = "uint256 name"
    let (result, s) = runParser pFunctionArgs str
    it "could parse the args" $ do
      result `shouldBe` Right [(STypeUint 256, "name")]
    it "should leave correct state" $ do
      s `shouldBe` ""
  describe "parse empty arg with quotes" $ do
    let str = "uint256 name, string newname"
    let (result, s) = runParser pFunctionArgs str
    it "could parse the args" $ do
      result
        `shouldBe` Right
          [ (STypeUint 256, "name"),
            (STypeString, "newname")
          ]
    it "should leave correct state" $ do
      s `shouldBe` ""

parseFunctionQuotedArgs :: Spec
parseFunctionQuotedArgs = do
  let testCases =
        [ ( "()",
            Right [],
            ""
          ),
          ( " (  ) ",
            Right [],
            ""
          ),
          ( " ( uint256 name) ",
            Right [(STypeUint 256, "name")],
            ""
          ),
          ( " ( uint256 name, string oldname) ",
            Right [(STypeUint 256, "name"), (STypeString, "oldname")],
            ""
          )
        ]
  forM_ testCases $ \(input, expectedResult, expectedState) -> do
    describe ("parse function args: " ++ input) $ do
      let (result, s) = runParser pFunctionArgsQuoted input
      it "could parse the args" $ do
        result `shouldBe` expectedResult
      it "should leave correct state" $ do
        s `shouldBe` expectedState

parseFunctionModifiers :: Spec
parseFunctionModifiers = do
  let testCases =
        [ ( "public  view",
            Right ["public", "view"],
            ""
          ),
          ( "public  view {",
            Right ["public", "view"],
            "{"
          ),
          ( "public {",
            Right ["public"],
            "{"
          ),
          ( "public  view returns {",
            Right ["public", "view"],
            "returns {"
          )
        ]
  forM_ testCases $ \(input, expectedResult, expectedState) -> do
    describe ("parse function modifiers: " ++ input) $ do
      let (result, s) = runParser pFunctionDecorators input
      it "could parse the modifiers" $ do
        result `shouldBe` expectedResult
      it "should leave correct state" $ do
        s `shouldBe` expectedState

parseFunctionReturnsClauseSpec :: Spec
parseFunctionReturnsClauseSpec = do
  describe "parse function modifiers with curly bracket and returns" $ do
    let str = "returns (uint256){"
    let (result, s) = runParser pReturnsClause str
    it "could parse the args" $ do
      result `shouldBe` Right (STypeUint 256)
    it "should leave correct state" $ do
      s `shouldBe` "{"

parseCommentSpec :: Spec
parseCommentSpec = do
  let testCases =
        [ ( "//helloworld _*&^",
            Right "helloworld _*&^",
            ""
          ),
          ( "//helloworld _*&^\n",
            Right "helloworld _*&^",
            ""
          ),
          ( "//   helloworld _*&^\n",
            Right "   helloworld _*&^",
            ""
          )
        ]
  forM_ testCases $ \(input, expectedResult, expectedState) -> do
    describe ("parse comment: " ++ input) $ do
      let (result, s) = runParser pComment input
      it "gets the correct comment string" $ do
        result `shouldBe` expectedResult
      it "leaves the correct state" $ do
        s `shouldBe` expectedState

parseSPDXCommentSpec :: Spec
parseSPDXCommentSpec = do
  let testCases =
        [ ( "// SPDX-License-Identifier: MIT",
            Right "MIT",
            ""
          ),
          ( "// SPDX-License-Identifier: BSD-2",
            Right "BSD-2",
            ""
          )
        ]
  forM_ testCases $ \(input, expectedResult, expectedState) -> do
    describe ("parse SPDX comment: " ++ input) $ do
      let (result, s) = runParser pSPDXComment input
      it "gets the correct comment string" $ do
        result `shouldBe` expectedResult
      it "leaves the correct state" $ do
        s `shouldBe` expectedState

parsePragmaSpec :: Spec
parsePragmaSpec = do
  let testCases =
        [ ( "pragma solidity ^0.8.24;",
            Right (SemVer {major = 0, minor = 8, patch = Just 24, semVerRangeMark = Just Caret}),
            ""
          ),
          ( "pragma solidity ~0.8.24;",
            Right (SemVer {major = 0, minor = 8, patch = Just 24, semVerRangeMark = Just Tilde}),
            ""
          ),
          ( "pragma solidity *;",
            Right (SemVer {major = 0, minor = 0, patch = Nothing, semVerRangeMark = Just Wildcards}),
            ""
          ),
          ( "pragma solidity 0.8.24;",
            Right (SemVer {major = 0, minor = 8, patch = Just 24, semVerRangeMark = Nothing}),
            ""
          )
        ]
  forM_ testCases $ \(input, expectedResult, expectedState) -> do
    describe ("parse pragma version: " ++ input) $ do
      let (result, s) = runParser pPragma input
      it "gets the correct pragma string" $ do
        result `shouldBe` expectedResult
      it "leaves the correct state" $ do
        s `shouldBe` expectedState

parseTypeAllSpec :: Spec
parseTypeAllSpec = do
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
