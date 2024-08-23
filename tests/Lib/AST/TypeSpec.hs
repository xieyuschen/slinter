{-# LANGUAGE OverloadedStrings #-}

module Lib.AST.TypeSpec where

import Control.Monad (forM_)
import Lib.AST.Model
  ( ArrayN (ArrayN, aElemType, aSize),
    BitLengthDesc (BitLength, BitLengthWithDecimal),
    ExprBinary (ExprBinary, bOperator, leftOperand, rightOperand),
    Literal (LNum),
    Mapping (Mapping, mKeyType, mValueType),
    Operator (ArithmeticExp),
    SExpr (SExprB, SExprL, SExprVar),
    SType
      ( STypeAddress,
        STypeAlias,
        STypeArray,
        STypeBytes,
        STypeCustom,
        STypeFixed,
        STypeInt,
        STypeMapping,
        STypePayableAddress,
        STypeStructure,
        STypeUFixed,
        STypeUint
      ),
    STypeEnum (STypeEnum, eelems, ename),
    Structure (Structure, structFields, structName),
    UserDefinedValueTypeDefinition (UserDefinedValueTypeDefinition, _userDefinedValueElemType, _userDefinedValueTypeName),
  )
import Lib.AST.Type
  ( pType,
    pTypeDefinition,
    pTypeEnum,
    pTypeStruct,
    pTypeWithDesc,
    pUserDefinedValueTypeDefinition,
  )
import Lib.TestCommon (exactlyParserVerifier)
import Test.Hspec (Spec)

spec :: Spec
spec = do
  parseTypeEnumSpec
  parseTypeAliasSpec
  parseTypeStructureSpec
  parseTypeDifinitionSpec
  parseTypeWithBitLengthSpec
  parseSimpleTypeSpec
  parseArrayMapSpec
  parseTypeCustomSpec

parseArrayMapSpec :: Spec
parseArrayMapSpec = do
  let testCases =
        [ ( "mapping (address => uint256) private _balances;",
            Right $
              STypeMapping $
                Mapping
                  { mKeyType = STypeAddress,
                    mValueType = STypeUint 256
                  },
            "private _balances;"
          ),
          ( "mapping (\naddress => mapping\n(address => uint256)) \nprivate _allowances;",
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
          ),
          ( "int[]",
            Right
              ( STypeArray
                  ArrayN
                    { aElemType = STypeInt 256,
                      aSize = Nothing
                    }
              ),
            ""
          ),
          ( "int[3]",
            Right
              ( STypeArray
                  ArrayN
                    { aElemType = STypeInt 256,
                      aSize = Just $ SExprL $ LNum 3
                    }
              ),
            ""
          ),
          ( "int[x]", -- this is an inavlid syntax, we will report it in syntex check
            Right (STypeArray (ArrayN {aElemType = STypeInt 256, aSize = Just (SExprVar "x")})),
            ""
          ),
          ( "int[1][]",
            Right
              ( STypeArray
                  ArrayN
                    { aElemType =
                        STypeArray
                          ArrayN
                            { aElemType = STypeInt 256,
                              aSize = Just $ SExprL $ LNum 1
                            },
                      aSize = Nothing
                    }
              ),
            ""
          ),
          ( "int[][2]",
            Right
              ( STypeArray
                  ArrayN
                    { aElemType =
                        STypeArray
                          ArrayN
                            { aElemType = STypeInt 256,
                              aSize = Nothing
                            },
                      aSize = Just $ SExprL $ LNum 2
                    }
              ),
            ""
          ),
          ( "int[][]",
            Right
              ( STypeArray
                  ArrayN
                    { aElemType =
                        STypeArray
                          ArrayN
                            { aElemType = STypeInt 256,
                              aSize = Nothing
                            },
                      aSize = Nothing
                    }
              ),
            ""
          ),
          ( "int[2**20]", -- todo: fix me
            Right
              ( STypeArray
                  ArrayN
                    { aElemType = STypeInt 256,
                      aSize =
                        Just
                          ( SExprB
                              ExprBinary
                                { leftOperand = SExprL (LNum 2),
                                  rightOperand = SExprL (LNum 20),
                                  bOperator = ArithmeticExp
                                }
                          )
                    }
              ),
            ""
          )
        ]
  forM_ testCases $ exactlyParserVerifier "map and array types" pType

parseSimpleTypeSpec :: Spec
parseSimpleTypeSpec = do
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
            Left ["digit", "digit", "fail to define type with desired pattern", "fail to define type with desired pattern"],
            "uint7"
          ),
          ( "int128",
            Right $ STypeInt 128,
            ""
          ),
          ( "int257",
            Left ["digit", "digit", "fail to define type with desired pattern", "fail to define type with desired pattern"],
            "int257"
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
            Left ["digit", "digit", "fail to define type with desired pattern", "fail to define type with desired pattern"],
            "fixed257x100"
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
            Left ["digit", "digit", "fail to define type with desired pattern", "fail to define type with desired pattern"],
            "bytes9"
          )
        ]
  forM_ testCases $ exactlyParserVerifier "simple types" pType

parseTypeWithBitLengthSpec :: Spec
parseTypeWithBitLengthSpec = do
  let testCases =
        [ ("uint", Right ("uint", Nothing), ""),
          ("uint8", Right ("uint", Just $ BitLength 8), ""),
          ("uint128", Right ("uint", Just $ BitLength 128), ""),
          ("int", Right ("int", Nothing), ""),
          ("int16", Right ("int", Just $ BitLength 16), ""),
          ("custom256", Right ("custom256", Nothing), ""),
          ("custom256type", Right ("custom256type", Nothing), ""),
          ("custom256x18", Right ("custom256x18", Nothing), ""),
          ("fixed256", Right ("fixed256", Nothing), ""),
          ("fixed", Right ("fixed", Nothing), ""),
          ("fixed256x80", Right ("fixed", Just $ BitLengthWithDecimal 256 80), ""),
          ("bytes", Right ("bytes", Nothing), ""),
          ("bytes3", Right ("bytes", Just $ BitLength 3), ""),
          ("bytes9", Right ("bytes", Just $ BitLength 9), "")
        ]
  forM_ testCases $ exactlyParserVerifier "type with bit length" pTypeWithDesc

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
          ( "enum TEST \n{ A1, a2, \nA3_, A_4 \n}",
            Right $
              STypeEnum
                { ename = "TEST",
                  eelems = ["A1", "a2", "A3_", "A_4"]
                },
            ""
          ),
          ( "enum\n TEST \n{ A1, a2, \nA3_, A_4 \n}",
            Right $
              STypeEnum
                { ename = "TEST",
                  eelems = ["A1", "a2", "A3_", "A_4"]
                },
            ""
          ),
          ( "enum TEST { } test state",
            Right (STypeEnum {ename = "TEST", eelems = []}),
            "test state"
          )
        ]
  forM_ testCases $ exactlyParserVerifier "type enum" pTypeEnum

parseTypeAliasSpec :: Spec
parseTypeAliasSpec = do
  let testCases =
        [ ( "type Alias1 is uint256;",
            Right $
              UserDefinedValueTypeDefinition
                { _userDefinedValueTypeName = "Alias1",
                  _userDefinedValueElemType = STypeUint 256
                },
            ""
          ),
          ( "type _Ab is address;",
            Right $
              UserDefinedValueTypeDefinition
                { _userDefinedValueTypeName = "_Ab",
                  _userDefinedValueElemType = STypeAddress
                },
            ""
          ),
          ( "type is uint256;",
            Left ["\"u\"", "\"u\"", "space", "\"is\""],
            "type is uint256;"
          )
        ]
  forM_ testCases $ exactlyParserVerifier "type alias" pUserDefinedValueTypeDefinition

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
            Left ["\";\"", "\";\"", "\";\"", "\";\"", "\";\"", "\";\"", "\";\"", "letter or digit", "\"_\"", "space", "\"payable\"", "space", "letter", "\"_\""],
            "struct empty { \n uint128 price; address;}"
          ),
          ( "struct {} test state",
            Left ["\"{\"", "\"{\"", "\"{\"", "space", "letter", "\"_\""],
            "struct {} test state"
          )
        ]
  forM_ testCases $ exactlyParserVerifier "type struct" pTypeStruct

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
          ( "type Alias1 is uint256;",
            Right $
              STypeAlias $
                UserDefinedValueTypeDefinition
                  { _userDefinedValueTypeName = "Alias1",
                    _userDefinedValueElemType = STypeUint 256
                  },
            ""
          )
        ]
  forM_ testCases $ exactlyParserVerifier "type definition" pTypeDefinition

parseTypeCustomSpec :: Spec
parseTypeCustomSpec = do
  let testCases =
        [ ( "BitMap",
            Right (STypeCustom "BitMap"),
            ""
          )
        ]
  forM_ testCases $ exactlyParserVerifier "type definition" pType
