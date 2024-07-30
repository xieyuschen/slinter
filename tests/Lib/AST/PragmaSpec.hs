{-# LANGUAGE OverloadedStrings #-}

module Lib.AST.PragmaSpec (spec) where

import Control.Monad (forM_)
import Lib.AST.Model
import Lib.AST.Pragma
import Lib.Parser
import Lib.TestCommon
import Test.Hspec (Spec)

spec :: Spec
spec = do
  -- comments test cases
  parseCommentSpec
  parseSPDXCommentSpec

  -- pragam test cases
  parsePragmaSpec

  -- import directive test cases
  parseImportDirecticeSpec
  parseImportAliasesSpec

  -- using directive test cases
  parseUsingDirectiveSpec
  parseUsingFieldAliasesSpec
  parseUsingTypeSpec

parseCommentSpec :: Spec
parseCommentSpec = do
  let testCases =
        [ ( "//helloworld _*&^\r\n",
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
          ),
          ( "// helloworld _*&^",
            Left ["lf new-line", "crlf new-line"], -- no ending line, won't be treated as a comment
            "// helloworld _*&^"
          )
        ]
  forM_ testCases $ verifyParser "comment" pComment

parseSPDXCommentSpec :: Spec
parseSPDXCommentSpec = do
  let testCases =
        [ ( "// SPDX-License-Identifier: MIT\n",
            Right "MIT",
            ""
          ),
          ( "// SPDX-License-Identifier: BSD-2\n",
            Right "BSD-2",
            ""
          )
        ]
  forM_ testCases $ verifyParser "spdx comment" pSPDXComment

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
  forM_ testCases $ verifyParser "pragma" pPragma

parseImportDirecticeSpec :: Spec
parseImportDirecticeSpec = do
  let testCases =
        [ ( "import \"./MyLibrary.sol\";",
            Right (ImportPath "./MyLibrary.sol"),
            ""
          ),
          ( "import \"./MyLibrary.sol\" as MyLib;",
            Right (ImportPathWithAlias "./MyLibrary.sol" "MyLib"),
            ""
          ),
          ( "import {MySymbol} from \"./MyLibrary.sol\";",
            Right (ImportSymbolAliases "./MyLibrary.sol" [SymbolAlias {symbolIdentifier = "MySymbol", symbolAlias = Nothing}]),
            ""
          ),
          ( "import {MySymbol as RenamedSymbol} from \"./MyLibrary.sol\";",
            Right (ImportSymbolAliases "./MyLibrary.sol" [SymbolAlias {symbolIdentifier = "MySymbol", symbolAlias = Just "RenamedSymbol"}]),
            ""
          ),
          ( "import{MySymbol as RenamedSymbol}from \"./MyLibrary.sol\";",
            Right (ImportSymbolAliases "./MyLibrary.sol" [SymbolAlias {symbolIdentifier = "MySymbol", symbolAlias = Just "RenamedSymbol"}]),
            ""
          ),
          ( "import * as MyLib from \"./MyLibrary.sol\";",
            Right (ImportAllSymbols "./MyLibrary.sol" "MyLib"),
            ""
          ),
          ( -- import and star is combined together, should error
            "import* as MyLib from \"./MyLibrary.sol\";",
            Left ["\"*\"", "\"*\"", "\"*\"", "\"*\"", "\"*\"", "space", "space", "space", "\"{\"", "space"],
            "import* as MyLib from \"./MyLibrary.sol\";"
          ),
          ( -- the from and imported path is combined together, should error
            "import * as MyLib from\"./MyLibrary.sol\";",
            Left ["\"\\\"\"", "space"],
            "import * as MyLib from\"./MyLibrary.sol\";"
          )
        ]
  forM_ testCases $ verifyParser "import directive" pImportDirective

parseImportAliasesSpec :: Spec
parseImportAliasesSpec = do
  let testCases =
        [ ( "{MySymbol}",
            Right [SymbolAlias {symbolIdentifier = "MySymbol", symbolAlias = Nothing}],
            ""
          ),
          ( "{MySymbol as RenamedSymbol}",
            Right [SymbolAlias {symbolIdentifier = "MySymbol", symbolAlias = Just "RenamedSymbol"}],
            ""
          ),
          ( "{MySymbol, MySymbol2}",
            Right
              [ SymbolAlias {symbolIdentifier = "MySymbol", symbolAlias = Nothing},
                SymbolAlias {symbolIdentifier = "MySymbol2", symbolAlias = Nothing}
              ],
            ""
          ),
          ( "{MySymbol as RenamedSymbol, MySymbol2 as RenamedSymbol2}",
            Right
              [ SymbolAlias {symbolIdentifier = "MySymbol", symbolAlias = Just "RenamedSymbol"},
                SymbolAlias {symbolIdentifier = "MySymbol2", symbolAlias = Just "RenamedSymbol2"}
              ],
            ""
          )
        ]
  forM_ testCases $ verifyParser "import aliases" pImportAliases

parseUsingDirectiveSpec :: Spec
parseUsingDirectiveSpec = do
  let testCases =
        [ ( "using {sub as -, neg as -, add as +} for Bitmap global;",
            Right
              ( UsingDirective
                  { usingDirectiveField =
                      UFUsingAliases
                        [ UsingAlias {uaIdentifierPath = ["sub"], uaUserDefinableOper = Just Minus},
                          UsingAlias {uaIdentifierPath = ["neg"], uaUserDefinableOper = Just Minus},
                          UsingAlias {uaIdentifierPath = ["add"], uaUserDefinableOper = Just ArithmeticAddition}
                        ],
                    usingType = UsingTp (STypeCustom "Bitmap"),
                    usingGlobal = True
                  }
              ),
            ""
          ),
          ( "using {sub as -, neg as -, add as +} for Bitmap;",
            Right
              ( UsingDirective
                  { usingDirectiveField =
                      UFUsingAliases
                        [ UsingAlias {uaIdentifierPath = ["sub"], uaUserDefinableOper = Just Minus},
                          UsingAlias {uaIdentifierPath = ["neg"], uaUserDefinableOper = Just Minus},
                          UsingAlias {uaIdentifierPath = ["add"], uaUserDefinableOper = Just ArithmeticAddition}
                        ],
                    usingType = UsingTp (STypeCustom "Bitmap"),
                    usingGlobal = False
                  }
              ),
            ""
          ),
          ( "using {clear_count} for User global;",
            Right
              ( UsingDirective
                  { usingDirectiveField =
                      UFUsingAliases
                        [ UsingAlias {uaIdentifierPath = ["clear_count"], uaUserDefinableOper = Nothing}
                        ],
                    usingType = UsingTp (STypeCustom "User"),
                    usingGlobal = True
                  }
              ),
            ""
          ),
          ( "using {mask, odd} for *;",
            Right
              ( UsingDirective
                  { usingDirectiveField =
                      UFUsingAliases
                        [ UsingAlias {uaIdentifierPath = ["mask"], uaUserDefinableOper = Nothing},
                          UsingAlias {uaIdentifierPath = ["odd"], uaUserDefinableOper = Nothing}
                        ],
                    usingType = UsingAll,
                    usingGlobal = False
                  }
              ),
            ""
          ),
          ( "using {mask, odd} for * global;",
            Right
              ( UsingDirective
                  { usingDirectiveField =
                      UFUsingAliases
                        [ UsingAlias {uaIdentifierPath = ["mask"], uaUserDefinableOper = Nothing},
                          UsingAlias {uaIdentifierPath = ["odd"], uaUserDefinableOper = Nothing}
                        ],
                    usingType = UsingAll,
                    usingGlobal = True
                  }
              ),
            ""
          ),
          ( "using a.b.c for Bitmap;",
            Right
              ( UsingDirective
                  { usingDirectiveField = UFIdentifierPath ["a", "b", "c"],
                    usingType = UsingTp (STypeCustom "Bitmap"),
                    usingGlobal = False
                  }
              ),
            ""
          ),
          -- todo: 'using Utils.AddressUtils for Utils.AddressUtils;' doesn't work, fix the pType
          ( "using Utils.AddressUtils for Utils;",
            Right
              ( UsingDirective
                  { usingDirectiveField = UFIdentifierPath ["Utils", "AddressUtils"],
                    usingType = UsingTp (STypeCustom "Utils"),
                    usingGlobal = False
                  }
              ),
            ""
          )
        ]
  forM_ testCases $ verifyParser "using directive" pUsingDirective

parseUsingFieldAliasesSpec :: Spec
parseUsingFieldAliasesSpec = do
  let testCases =
        [ ( "{a.b.c, xx2 as &}",
            Right
              ( UFUsingAliases
                  [ UsingAlias
                      { uaIdentifierPath = ["a", "b", "c"],
                        uaUserDefinableOper = Nothing
                      },
                    UsingAlias
                      { uaIdentifierPath = ["xx2"],
                        uaUserDefinableOper = Just BitAnd
                      }
                  ]
              ),
            ""
          ),
          ( "{a.b.c  , xx2 as &}",
            Right
              ( UFUsingAliases
                  [ UsingAlias
                      { uaIdentifierPath = ["a", "b", "c"],
                        uaUserDefinableOper = Nothing
                      },
                    UsingAlias
                      { uaIdentifierPath = ["xx2"],
                        uaUserDefinableOper = Just BitAnd
                      }
                  ]
              ),
            ""
          ),
          ( "{a.b as +, xx}",
            Right
              ( UFUsingAliases
                  [ UsingAlias
                      { uaIdentifierPath = ["a", "b"],
                        uaUserDefinableOper = Just ArithmeticAddition
                      },
                    UsingAlias
                      { uaIdentifierPath = ["xx"],
                        uaUserDefinableOper = Nothing
                      }
                  ]
              ),
            ""
          ),
          ( "{a as +, xx}",
            Right
              ( UFUsingAliases
                  [ UsingAlias
                      { uaIdentifierPath = ["a"],
                        uaUserDefinableOper = Just ArithmeticAddition
                      },
                    UsingAlias
                      { uaIdentifierPath = ["xx"],
                        uaUserDefinableOper = Nothing
                      }
                  ]
              ),
            ""
          ),
          ( "{}",
            Right $
              UFUsingAliases [],
            ""
          ),
          ( "a.b.c",
            Right $
              UFIdentifierPath ["a", "b", "c"],
            ""
          )
        ]
  forM_ testCases $ verifyParser "using field aliases" pUsingField

parseUsingTypeSpec :: Spec
parseUsingTypeSpec = do
  let testCases =
        [ ( "User",
            Right (UsingTp (STypeCustom "User")),
            ""
          ),
          ( "Bitmap",
            Right (UsingTp (STypeCustom "Bitmap")),
            ""
          ),
          ( "*",
            Right UsingAll,
            ""
          )
        ]
  forM_ testCases $ verifyParser "using type" pUsingType
