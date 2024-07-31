{-# LANGUAGE OverloadedStrings #-}

module Lib.AST.DefinitionSpec (spec) where

import Control.Monad (forM_)
import Lib.AST.Definition (pEventDefinition, pModifierDefinition)
import Lib.AST.Model
import Lib.TestCommon (verifyParser)
import Test.Hspec (Spec)

spec :: Spec
spec = do
  parseModifierDefinitionSpec
  parseEventDefinitionSpec

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
          )
        ]

  forM_ testCases $ verifyParser "modifier definition" pModifierDefinition

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
          )
        ]

  forM_ testCases $ verifyParser "event definition" pEventDefinition
