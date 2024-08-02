{-# LANGUAGE OverloadedStrings #-}

module Lib.AST.UtilSpec (spec) where

import Control.Monad (forM_)
import Lib.AST.Model
import Lib.AST.Util
import Lib.TestCommon (exactlyParserVerifier)
import Test.Hspec

spec :: Spec
spec = do
  parseFunctionQuotedArgs

parseFunctionQuotedArgs :: Spec
parseFunctionQuotedArgs = do
  let fnResult =
        Right
          [ FnDeclArg
              { fnArgTp = STypeString,
                fnArgName = Just "str",
                fnArgLocation = Storage
              }
          ]
  let testCases =
        [ ( "()",
            Right [],
            ""
          ),
          ( "(\n)",
            Right [],
            ""
          ),
          ( " (  ) ",
            Right [],
            ""
          ),
          ( " (string str) ",
            fnResult,
            ""
          ),
          ( " (string \nstr) ",
            fnResult,
            ""
          ),
          ( " (\nstring str\n) ",
            fnResult,
            ""
          ),
          ( " (\nstring \nstr\n) ",
            fnResult,
            ""
          ),
          ( " ( uint256 name) ",
            Right
              [ FnDeclArg
                  { fnArgTp = STypeUint 256,
                    fnArgName = Just "name",
                    fnArgLocation = Storage
                  }
              ],
            ""
          ),
          ( "(uint256 name, string new_name)",
            Right
              [ FnDeclArg
                  { fnArgTp = STypeUint 256,
                    fnArgName = Just "name",
                    fnArgLocation = Storage
                  },
                FnDeclArg
                  { fnArgTp = STypeString,
                    fnArgName = Just "new_name",
                    fnArgLocation = Storage
                  }
              ],
            ""
          ),
          ( "(uint256 memory \nname, string calldata\n new_name, string)",
            Right
              [ FnDeclArg
                  { fnArgTp = STypeUint 256,
                    fnArgName = Just "name",
                    fnArgLocation = Memory
                  },
                FnDeclArg
                  { fnArgTp = STypeString,
                    fnArgName = Just "new_name",
                    fnArgLocation = Calldata
                  },
                FnDeclArg
                  { fnArgTp = STypeString,
                    fnArgName = Nothing,
                    fnArgLocation = Storage
                  }
              ],
            ""
          ),
          ( " ( uint256 name\n, string old_name,\n fixed256x16) ",
            Right
              [ FnDeclArg
                  { fnArgTp = STypeUint 256,
                    fnArgName = Just "name",
                    fnArgLocation = Storage
                  },
                FnDeclArg
                  { fnArgTp = STypeString,
                    fnArgName = Just "old_name",
                    fnArgLocation = Storage
                  },
                FnDeclArg
                  { fnArgTp = STypeFixed 256 16,
                    fnArgName = Nothing,
                    fnArgLocation = Storage
                  }
              ],
            ""
          )
        ]

  forM_ testCases $ exactlyParserVerifier "function args quoted" pFnDeclArgsInParentheses
