{-# LANGUAGE OverloadedStrings #-}

module Lib.AST.DefinitionSpec (spec) where

import Control.Monad (forM_)
import Lib.AST.Definition (pContractBody, pErrorDefinition, pEventDefinition, pInterfaceDefinition, pLibraryDefinition, pModifierDefinition)
import Lib.AST.Model
import Lib.TestCommon (exactlyParserVerifier, leftRightJustifier, newParserVerifier, resultIsRight)
import Test.Hspec (Spec)

spec :: Spec
spec = do
  parseModifierDefinitionSpec
  parseEventDefinitionSpec
  parseContractBodySpec
  parseErrorDefintionSpec
  parseInterfaceDefintionSpec
  parseLibraryDefinitionSpec

parseModifierDefinitionSpec :: Spec
parseModifierDefinitionSpec = do
  let testCases =
        [ ( "modifier ExampleM;",
            Right
              ( ModifierDefinition
                  { modifierName = "ExampleM",
                    modifierParamList = Nothing,
                    modifierIsVirtual = False,
                    modifierOverrideSpecifier = Nothing,
                    modifierBody = Nothing
                  }
              ),
            ""
          ),
          ( "modifier ExampleM{}",
            Right
              ( ModifierDefinition
                  { modifierName = "ExampleM",
                    modifierParamList = Nothing,
                    modifierIsVirtual = False,
                    modifierOverrideSpecifier = Nothing,
                    modifierBody = Just []
                  }
              ),
            ""
          ),
          ( "modifier ExampleM virtual;",
            Right
              ( ModifierDefinition
                  { modifierName = "ExampleM",
                    modifierParamList = Nothing,
                    modifierIsVirtual = True,
                    modifierOverrideSpecifier = Nothing,
                    modifierBody = Nothing
                  }
              ),
            ""
          ),
          ( "modifier ExampleM override;",
            Right
              ( ModifierDefinition
                  { modifierName = "ExampleM",
                    modifierParamList = Nothing,
                    modifierIsVirtual = False,
                    modifierOverrideSpecifier = Just [],
                    modifierBody = Nothing
                  }
              ),
            ""
          ),
          ( "modifier ExampleM virtual override;",
            Right
              ( ModifierDefinition
                  { modifierName = "ExampleM",
                    modifierParamList = Nothing,
                    modifierIsVirtual = True,
                    modifierOverrideSpecifier = Just [],
                    modifierBody = Nothing
                  }
              ),
            ""
          ),
          ( "modifier ExampleM override virtual;",
            Right
              ( ModifierDefinition
                  { modifierName = "ExampleM",
                    modifierParamList = Nothing,
                    modifierIsVirtual = True,
                    modifierOverrideSpecifier = Just [],
                    modifierBody = Nothing
                  }
              ),
            ""
          ),
          ( "modifier ExampleM override(a.b.c, d.e);",
            Right
              ( ModifierDefinition
                  { modifierName = "ExampleM",
                    modifierParamList = Nothing,
                    modifierIsVirtual = False,
                    modifierOverrideSpecifier = Just [["a", "b", "c"], ["d", "e"]],
                    modifierBody = Nothing
                  }
              ),
            ""
          ),
          ( "modifier ExampleM(uint256 memory hello, string str);",
            Right
              ( ModifierDefinition
                  { modifierName = "ExampleM",
                    modifierParamList =
                      Just
                        [ FnDeclArg {fnArgTp = STypeUint 256, fnArgName = Just "hello", fnArgLocation = Memory},
                          FnDeclArg {fnArgTp = STypeString, fnArgName = Just "str", fnArgLocation = Storage}
                        ],
                    modifierIsVirtual = False,
                    modifierOverrideSpecifier = Nothing,
                    modifierBody = Nothing
                  }
              ),
            ""
          ),
          ( "modifierExampleM(uint256 memory hello, string str);",
            Left ["\"E\"", "space", "space is required after keyword 'modifier'"],
            "modifierExampleM(uint256 memory hello, string str);"
          )
        ]

  forM_ testCases $ exactlyParserVerifier "modifier definition" pModifierDefinition

parseEventDefinitionSpec :: Spec
parseEventDefinitionSpec = do
  let testCases =
        [ ( "event EA();",
            Right
              ( EventDefinition
                  { eventName = "EA",
                    eventParameters = [],
                    eventIsAnonymous = False
                  }
              ),
            ""
          ),
          ( "event EA() anonymous;",
            Right
              ( EventDefinition
                  { eventName = "EA",
                    eventParameters = [],
                    eventIsAnonymous = True
                  }
              ),
            ""
          ),
          ( "event EA();",
            Right
              ( EventDefinition
                  { eventName = "EA",
                    eventParameters = [],
                    eventIsAnonymous = False
                  }
              ),
            ""
          ),
          ( "event EA(uint256);",
            Right
              ( EventDefinition
                  { eventName = "EA",
                    eventParameters =
                      [ EventParameter
                          { eventParamType = STypeUint 256,
                            eventParamIndex = Nothing,
                            eventParamIdent = Nothing
                          }
                      ],
                    eventIsAnonymous = False
                  }
              ),
            ""
          ),
          ( "event EA(string 0);",
            Right
              ( EventDefinition
                  { eventName = "EA",
                    eventParameters =
                      [ EventParameter
                          { eventParamType = STypeString,
                            eventParamIndex = Just 0,
                            eventParamIdent = Nothing
                          }
                      ],
                    eventIsAnonymous = False
                  }
              ),
            ""
          ),
          ( "event EA(string 0 str);",
            Right
              ( EventDefinition
                  { eventName = "EA",
                    eventParameters =
                      [ EventParameter
                          { eventParamType = STypeString,
                            eventParamIndex = Just 0,
                            eventParamIdent = Just "str"
                          }
                      ],
                    eventIsAnonymous = False
                  }
              ),
            ""
          ),
          ( "event EA(string 0 str, string) anonymous;",
            Right
              ( EventDefinition
                  { eventName = "EA",
                    eventParameters =
                      [ EventParameter
                          { eventParamType = STypeString,
                            eventParamIndex = Just 0,
                            eventParamIdent = Just "str"
                          },
                        EventParameter
                          { eventParamType = STypeString,
                            eventParamIndex = Nothing,
                            eventParamIdent = Nothing
                          }
                      ],
                    eventIsAnonymous = True
                  }
              ),
            ""
          ),
          ( "eventEA(string 0 str, string) anonymous;",
            Left ["\"E\"", "space", "space is required after keyword 'event'"],
            "eventEA(string 0 str, string) anonymous;"
          )
        ]

  forM_ testCases $ exactlyParserVerifier "event definition" pEventDefinition

parseContractBodySpec :: Spec
parseContractBodySpec = do
  let testCases =
        [ ( "function inc(string name) public pure { count += 1; }",
            resultIsRight,
            ""
          ),
          ( "function inc(string name) public pure { count += 1; } \
            \modifier ExampleM(uint256 memory hello, string str); \
            \constructor() {} \
            \struct empty { \n uint128 price; \n address addr; \n }  \
            \enum TEST { A1, a2, A3_, A_4 } \
            \type _Ab is address; \
            \uint256 constant name = hello;\
            \event EA(string 0 str, string) anonymous; \
            \event EA(string 0 str); \
            \using a.b.c for Bitmap;\
            \function receive(string name) public pure { count += 1; } \
            \function fallback(string name) public pure { count += 1; } \
            \ ",
            resultIsRight,
            ""
          ),
          ( "function inc(string name) public pure { count += 1; } \n\
            \modifier ExampleM(uint256 memory hello, string str); \n\
            \constructor() {} \n\
            \struct empty { \n uint128 price; \n address addr; \n }  \n\
            \enum TEST { A1, a2, A3_, A_4 } \n\
            \type _Ab is address; \n\
            \uint256 constant name = hello;\n \
            \event EA(string 0 str, string) anonymous; \n\
            \event EA(string 0 str); \n\
            \using a.b.c for Bitmap;\n\
            \function receive(string name) public pure { count += 1; } \n\
            \function fallback(string name) public pure { count += 1; } \n\
            \function getResult() external view returns(uint);\n\
            \ ",
            resultIsRight,
            ""
          )
        ]

  -- we won't put the
  forM_ testCases $ newParserVerifier leftRightJustifier "contract body" pContractBody

parseErrorDefintionSpec :: Spec
parseErrorDefintionSpec = do
  let testCases =
        [ ( "error Unauthorized();",
            Right (ErrorDefinition {errName = "Unauthorized", errParameters = []}),
            ""
          ),
          ( "error InsufficientBalance(uint256 available, uint256 required);",
            Right (ErrorDefinition {errName = "InsufficientBalance", errParameters = [ErrorParameter {errParamType = STypeUint 256, errParamName = Just "available"}, ErrorParameter {errParamType = STypeUint 256, errParamName = Just "required"}]}),
            ""
          ),
          ( "errorUnauthorized();",
            Left ["\"U\"", "space", "space is required after keyword 'error'"],
            "errorUnauthorized();"
          )
        ]

  forM_ testCases $ exactlyParserVerifier "modifier definition" pErrorDefinition

parseInterfaceDefintionSpec :: Spec
parseInterfaceDefintionSpec = do
  let testCases =
        [ ( "interface Calculator { \n \
            \ function getResult() external view returns(uint); \
            \ }",
            Right
              ( InterfaceDefinition
                  { interfaceName = "Calculator",
                    interfaceInheritanceSpecifiers = [],
                    interfaceBody =
                      ContractBody
                        { ctBodyConstructor = Nothing,
                          ctBodyFunctions = [FunctionDefinition {fnDefName = FnNormal "getResult", fnState = FnStateView, fnVisibility = FnExternal, fnModifierInvocations = [], fnFnOverrideSpecifier = Nothing, fnIsVirtual = False, fargs = [], fnReturnTyp = Just (STypeUint 256), fnBody = Nothing}],
                          ctBodyModifiers = [],
                          ctBodyFallbackFunctions = [],
                          ctBodyReceiveFunctions = [],
                          ctBodyStructDefinitions = [],
                          ctBodyEnumDefinitions = [],
                          ctBodyUserDefinedValueTypeDefinition = [],
                          ctBodyStateVariables = [],
                          ctBodyEventDefinitions = [],
                          ctBodyErrorDefinitions = [],
                          ctBodyUsingDirectives = []
                          -- ctBodyAllFields = [CBFSSumFunctionDefinition (FunctionDefinition {fnDefName = FnNormal "getResult", fnState = FnStateView, fnVisibility = FnExternal, fnModifierInvocations = [], fnFnOverrideSpecifier = Nothing, fnIsVirtual = False, fargs = [], fnReturnTyp = Just (STypeUint 256), fnBody = Nothing})]
                        }
                  }
              ),
            ""
          ),
          ( "interface Calculator is MyInterface { \n \
            \ function getResult() external view returns(uint); \
            \ }",
            Right
              ( InterfaceDefinition
                  { interfaceName = "Calculator",
                    interfaceInheritanceSpecifiers = [InheritanceSpecifier {inheritancePath = ["MyInterface"], inheritanceCallArgs = Nothing}],
                    interfaceBody =
                      ContractBody
                        { ctBodyConstructor = Nothing,
                          ctBodyFunctions = [FunctionDefinition {fnDefName = FnNormal "getResult", fnState = FnStateView, fnVisibility = FnExternal, fnModifierInvocations = [], fnFnOverrideSpecifier = Nothing, fnIsVirtual = False, fargs = [], fnReturnTyp = Just (STypeUint 256), fnBody = Nothing}],
                          ctBodyModifiers = [],
                          ctBodyFallbackFunctions = [],
                          ctBodyReceiveFunctions = [],
                          ctBodyStructDefinitions = [],
                          ctBodyEnumDefinitions = [],
                          ctBodyUserDefinedValueTypeDefinition = [],
                          ctBodyStateVariables = [],
                          ctBodyEventDefinitions = [],
                          ctBodyErrorDefinitions = [],
                          ctBodyUsingDirectives = []
                          -- ctBodyAllFields = [CBFSSumFunctionDefinition (FunctionDefinition {fnDefName = FnNormal "getResult", fnState = FnStateView, fnVisibility = FnExternal, fnModifierInvocations = [], fnFnOverrideSpecifier = Nothing, fnIsVirtual = False, fargs = [], fnReturnTyp = Just (STypeUint 256), fnBody = Nothing})]
                        }
                  }
              ),
            ""
          ),
          ( "interface Calculator is MyInterface(123, \"hello\"), A.MyInterface() { \n \
            \ function getResult() external view returns(uint); \
            \ }",
            Right
              ( InterfaceDefinition
                  { interfaceName = "Calculator",
                    interfaceInheritanceSpecifiers =
                      [ InheritanceSpecifier {inheritancePath = ["MyInterface"], inheritanceCallArgs = Just (FnCallArgsList [SExprL (LNum 123), SExprL (LString "hello")])},
                        InheritanceSpecifier
                          { inheritancePath = ["A", "MyInterface"],
                            inheritanceCallArgs = Just (FnCallArgsList [])
                          }
                      ],
                    interfaceBody =
                      ContractBody
                        { ctBodyConstructor = Nothing,
                          ctBodyFunctions = [FunctionDefinition {fnDefName = FnNormal "getResult", fnState = FnStateView, fnVisibility = FnExternal, fnModifierInvocations = [], fnFnOverrideSpecifier = Nothing, fnIsVirtual = False, fargs = [], fnReturnTyp = Just (STypeUint 256), fnBody = Nothing}],
                          ctBodyModifiers = [],
                          ctBodyFallbackFunctions = [],
                          ctBodyReceiveFunctions = [],
                          ctBodyStructDefinitions = [],
                          ctBodyEnumDefinitions = [],
                          ctBodyUserDefinedValueTypeDefinition = [],
                          ctBodyStateVariables = [],
                          ctBodyEventDefinitions = [],
                          ctBodyErrorDefinitions = [],
                          ctBodyUsingDirectives = []
                          -- ctBodyAllFields = [CBFSSumFunctionDefinition (FunctionDefinition {fnDefName = FnNormal "getResult", fnState = FnStateView, fnVisibility = FnExternal, fnModifierInvocations = [], fnFnOverrideSpecifier = Nothing, fnIsVirtual = False, fargs = [], fnReturnTyp = Just (STypeUint 256), fnBody = Nothing})]
                        }
                  }
              ),
            ""
          ),
          ( "interfaceABC();",
            Left ["\"A\"", "space", "space is required after keyword 'interface'"],
            "interfaceABC();"
          )
        ]

  forM_ testCases $ exactlyParserVerifier "interface definition" pInterfaceDefinition

parseLibraryDefinitionSpec :: Spec
parseLibraryDefinitionSpec = do
  let testCases =
        [ ( "library Lib{}",
            Right (LibraryDefinition {libraryName = "Lib", libraryBody = ContractBody {ctBodyConstructor = Nothing, ctBodyFunctions = [], ctBodyModifiers = [], ctBodyFallbackFunctions = [], ctBodyReceiveFunctions = [], ctBodyStructDefinitions = [], ctBodyEnumDefinitions = [], ctBodyUserDefinedValueTypeDefinition = [], ctBodyStateVariables = [], ctBodyEventDefinitions = [], ctBodyErrorDefinitions = [], ctBodyUsingDirectives = []
            -- , ctBodyAllFields = []
            }}),
            ""
          ),
          ( "library Lib{function getResult() external view returns(uint);}",
            Right
              ( LibraryDefinition
                  { libraryName = "Lib",
                    libraryBody =
                      ContractBody
                        { ctBodyConstructor = Nothing,
                          ctBodyFunctions = [FunctionDefinition {fnDefName = FnNormal "getResult", fnState = FnStateView, fnVisibility = FnExternal, fnModifierInvocations = [], fnFnOverrideSpecifier = Nothing, fnIsVirtual = False, fargs = [], fnReturnTyp = Just (STypeUint 256), fnBody = Nothing}],
                          ctBodyModifiers = [],
                          ctBodyFallbackFunctions = [],
                          ctBodyReceiveFunctions = [],
                          ctBodyStructDefinitions = [],
                          ctBodyEnumDefinitions = [],
                          ctBodyUserDefinedValueTypeDefinition = [],
                          ctBodyStateVariables = [],
                          ctBodyEventDefinitions = [],
                          ctBodyErrorDefinitions = [],
                          ctBodyUsingDirectives = []
                          -- ctBodyAllFields = [CBFSSumFunctionDefinition (FunctionDefinition {fnDefName = FnNormal "getResult", fnState = FnStateView, fnVisibility = FnExternal, fnModifierInvocations = [], fnFnOverrideSpecifier = Nothing, fnIsVirtual = False, fargs = [], fnReturnTyp = Just (STypeUint 256), fnBody = Nothing})]
                        }
                  }
              ),
            ""
          ),
          ( "libraryLib{}",
            Left ["\"L\"", "space", "space is required after keyword 'library'"],
            "libraryLib{}"
          )
        ]
  forM_ testCases $ exactlyParserVerifier "library definition" pLibraryDefinition
