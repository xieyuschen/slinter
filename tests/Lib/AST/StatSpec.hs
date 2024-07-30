{-# LANGUAGE OverloadedStrings #-}

module Lib.AST.StatSpec (spec) where

import Control.Monad (forM_)
import Lib.AST.Model
import Lib.AST.Stat
import Lib.TestCommon (verifyParser)
import Test.Hspec (Spec)

spec :: Spec
spec = do
  parseStatAssignSpec
  parseVarDefinitionSpec
  parseStateVarSpec
  parseStateIfElseSpec
  parseForStatementSpec
  parseWhileStatementSpec
  parseDoWhileStatementSpec
  parseTryCatchStatementSpec
  parseCatchStatementSpec
  parseEmitStatementSpec
  parseRevertStatementSpec

parseVarDefinitionSpec :: Spec
parseVarDefinitionSpec = do
  let testCases =
        [ ( "uint256 public name = hello;",
            Left ["\"n\"", "\"n\"", "space", "\"=\""],
            "uint256 public name = hello;"
          ),
          ( "fixed hello=2345;",
            Right
              StVarDefStatement
                { stVarType = STypeFixed 128 18,
                  stVarName = "hello",
                  stVarLocation = Storage,
                  stVarExpr = Just (SExprL (LNum 2345)),
                  stVarComment = Nothing
                },
            ""
          ),
          ( "fixed memory hello=2345;",
            Right
              StVarDefStatement
                { stVarType = STypeFixed 128 18,
                  stVarName = "hello",
                  stVarLocation = Memory,
                  stVarExpr = Just (SExprL (LNum 2345)),
                  stVarComment = Nothing
                },
            ""
          )
        ]
  forM_ testCases $ verifyParser "variable definition" pStVarDefStatement

parseStateVarSpec :: Spec
parseStateVarSpec = do
  let testCases =
        [ ( "uint256 public name;",
            Right
              StateVariable
                { svVisibleSpecifier = FnPublic,
                  svType = STypeUint 256,
                  svName = "name",
                  svComment = Nothing,
                  svVarExpr = Nothing
                },
            ""
          ),
          ( "uint256 public name; // 123daws\n",
            Right
              StateVariable
                { svVisibleSpecifier = FnPublic,
                  svType = STypeUint 256,
                  svName = "name",
                  svComment = Just " 123daws",
                  svVarExpr = Nothing
                },
            ""
          ),
          ( "uint256 public name = hello;",
            Right
              StateVariable
                { svVisibleSpecifier = FnPublic,
                  svType = STypeUint 256,
                  svName = "name",
                  svComment = Nothing,
                  svVarExpr = Just (SExprVar "hello")
                },
            ""
          ),
          ( "fixed hello=2345;",
            Right
              StateVariable
                { svVisibleSpecifier = FnInternal,
                  svType = STypeFixed 128 18,
                  svName = "hello",
                  svComment = Nothing,
                  svVarExpr = Just (SExprL (LNum 2345))
                },
            ""
          ),
          -- state variable rejects the memory modifier
          ( "fixed memory invalid_state_var=2345;",
            Left ["\"i\"", "\"i\"", "space", "\"=\""],
            "fixed memory invalid_state_var=2345;"
          )
        ]
  forM_ testCases $ verifyParser "state variable definition" pStateVariable

parseStatAssignSpec :: Spec
parseStatAssignSpec = do
  let testCases =
        [ ( "owner = msg.sender;",
            Right $
              StAssignStatement
                { stAssignVarName = "owner",
                  stAssignExpr =
                    SExprS $
                      ExprSelection
                        { selectionBase = SExprVar "msg",
                          selectionField = "sender"
                        }
                },
            ""
          ),
          ( "owner = 1+2-3;",
            Right
              StAssignStatement
                { stAssignVarName = "owner",
                  stAssignExpr =
                    SExprB
                      ExprBinary
                        { leftOperand =
                            SExprB
                              ExprBinary
                                { leftOperand = SExprL (LNum 1),
                                  rightOperand = SExprL (LNum 2),
                                  bOperator = ArithmeticAddition
                                },
                          rightOperand = SExprL (LNum 3),
                          bOperator = Minus
                        }
                },
            ""
          ),
          ( "owner = msg[x].sender;",
            Right
              StAssignStatement
                { stAssignVarName = "owner",
                  stAssignExpr =
                    SExprS
                      ExprSelection
                        { selectionBase =
                            SExprI
                              ExprIndex
                                { elemBase = SExprVar "msg",
                                  elemIndex = SExprVar "x"
                                },
                          selectionField = "sender"
                        }
                },
            ""
          ),
          ( "owner = ctname.sender();",
            Right
              StAssignStatement
                { stAssignVarName = "owner",
                  stAssignExpr =
                    SExprF
                      ExprFnCall
                        { fnContractName = Just "ctname",
                          fnName = "sender",
                          fnArguments = FnCallArgsList []
                        }
                },
            ""
          ),
          ( "owner = ctname.sender()",
            Left
              [ "space",
                "space",
                "space",
                "space",
                "space",
                "space",
                "space",
                "space",
                "space",
                "space",
                "space",
                "space",
                "space",
                "space",
                "space",
                "space",
                "space",
                "space",
                "space",
                "space",
                "space",
                "space",
                "space",
                "space",
                "space",
                "space",
                "space",
                "\";\""
              ],
            "owner = ctname.sender()"
          )
        ]

  forM_ testCases $ verifyParser "assign statement" pAssignStat

parseStateIfElseSpec :: Spec
parseStateIfElseSpec = do
  let testCases =
        [ ( "if (amount > msg.value / 2 ) a=3;",
            Right
              ( IfStatement
                  { stIfCond =
                      SExprB
                        ( ExprBinary
                            { leftOperand = SExprVar "amount",
                              rightOperand =
                                SExprB
                                  ( ExprBinary
                                      { leftOperand = SExprS (ExprSelection {selectionBase = SExprVar "msg", selectionField = "value"}),
                                        rightOperand = SExprL (LNum 2),
                                        bOperator = ArithmeticDivision
                                      }
                                  ),
                              bOperator = ComparisionMore
                            }
                        ),
                    stIfThen =
                      [ StatAssign
                          ( StAssignStatement
                              { stAssignVarName = "a",
                                stAssignExpr = SExprL (LNum 3)
                              }
                          )
                      ],
                    stIfElse = []
                  }
              ),
            ""
          ),
          ( "if (amount > msg.value / 2 ) a=3; a=4;",
            -- a=3 belongs to the if scope, but a=4 not
            Right
              ( IfStatement
                  { stIfCond =
                      SExprB
                        ( ExprBinary
                            { leftOperand = SExprVar "amount",
                              rightOperand =
                                SExprB
                                  ( ExprBinary
                                      { leftOperand = SExprS (ExprSelection {selectionBase = SExprVar "msg", selectionField = "value"}),
                                        rightOperand = SExprL (LNum 2),
                                        bOperator = ArithmeticDivision
                                      }
                                  ),
                              bOperator = ComparisionMore
                            }
                        ),
                    stIfThen =
                      [ StatAssign
                          ( StAssignStatement
                              { stAssignVarName = "a",
                                stAssignExpr = SExprL (LNum 3)
                              }
                          )
                      ],
                    stIfElse = []
                  }
              ),
            " a=4;"
          ),
          ( "if (amount > msg.value / 2 ) {a=3;}",
            Right
              ( IfStatement
                  { stIfCond =
                      SExprB
                        ( ExprBinary
                            { leftOperand = SExprVar "amount",
                              rightOperand =
                                SExprB
                                  ( ExprBinary
                                      { leftOperand = SExprS (ExprSelection {selectionBase = SExprVar "msg", selectionField = "value"}),
                                        rightOperand = SExprL (LNum 2),
                                        bOperator = ArithmeticDivision
                                      }
                                  ),
                              bOperator = ComparisionMore
                            }
                        ),
                    stIfThen =
                      [ StatAssign
                          ( StAssignStatement
                              { stAssignVarName = "a",
                                stAssignExpr = SExprL (LNum 3)
                              }
                          )
                      ],
                    stIfElse = []
                  }
              ),
            ""
          ),
          ( "if (amount > msg.value / 2 ) {a=3; a=4;}",
            -- a=3 belongs to the if scope, but a=4 not
            Right
              ( IfStatement
                  { stIfCond =
                      SExprB
                        ( ExprBinary
                            { leftOperand = SExprVar "amount",
                              rightOperand =
                                SExprB
                                  ( ExprBinary
                                      { leftOperand = SExprS (ExprSelection {selectionBase = SExprVar "msg", selectionField = "value"}),
                                        rightOperand = SExprL (LNum 2),
                                        bOperator = ArithmeticDivision
                                      }
                                  ),
                              bOperator = ComparisionMore
                            }
                        ),
                    stIfThen =
                      [ StatAssign
                          ( StAssignStatement
                              { stAssignVarName = "a",
                                stAssignExpr = SExprL (LNum 3)
                              }
                          ),
                        StatAssign
                          StAssignStatement
                            { stAssignVarName = "a",
                              stAssignExpr = SExprL (LNum 4)
                            }
                      ],
                    stIfElse = []
                  }
              ),
            ""
          ),
          ( "if (amount > msg.value / 2 ) {a=3;} else { a=4;}",
            -- a=3 belongs to the if scope, but a=4 not
            Right
              ( IfStatement
                  { stIfCond =
                      SExprB
                        ( ExprBinary
                            { leftOperand = SExprVar "amount",
                              rightOperand =
                                SExprB
                                  ( ExprBinary
                                      { leftOperand = SExprS (ExprSelection {selectionBase = SExprVar "msg", selectionField = "value"}),
                                        rightOperand = SExprL (LNum 2),
                                        bOperator = ArithmeticDivision
                                      }
                                  ),
                              bOperator = ComparisionMore
                            }
                        ),
                    stIfThen =
                      [ StatAssign
                          ( StAssignStatement
                              { stAssignVarName = "a",
                                stAssignExpr = SExprL (LNum 3)
                              }
                          )
                      ],
                    stIfElse =
                      [ StatAssign
                          StAssignStatement
                            { stAssignVarName = "a",
                              stAssignExpr = SExprL (LNum 4)
                            }
                      ]
                  }
              ),
            ""
          ),
          ( "if (amount > msg.value / 2 ) {a=3;} else if (amount > msg.value) { a=4;} else {a=5;}",
            -- a=3 belongs to the if scope, but a=4 not
            Right
              ( IfStatement
                  { stIfCond =
                      SExprB
                        ( ExprBinary
                            { leftOperand = SExprVar "amount",
                              rightOperand =
                                SExprB
                                  ( ExprBinary
                                      { leftOperand = SExprS (ExprSelection {selectionBase = SExprVar "msg", selectionField = "value"}),
                                        rightOperand = SExprL (LNum 2),
                                        bOperator = ArithmeticDivision
                                      }
                                  ),
                              bOperator = ComparisionMore
                            }
                        ),
                    stIfThen =
                      [ StatAssign
                          ( StAssignStatement
                              { stAssignVarName = "a",
                                stAssignExpr = SExprL (LNum 3)
                              }
                          )
                      ],
                    stIfElse =
                      [ StatIf
                          IfStatement
                            { stIfCond =
                                SExprB
                                  ExprBinary
                                    { leftOperand = SExprVar "amount",
                                      rightOperand =
                                        SExprS
                                          ExprSelection
                                            { selectionBase = SExprVar "msg",
                                              selectionField = "value"
                                            },
                                      bOperator = ComparisionMore
                                    },
                              stIfThen =
                                [ StatAssign
                                    StAssignStatement
                                      { stAssignVarName = "a",
                                        stAssignExpr = SExprL (LNum 4)
                                      }
                                ],
                              stIfElse =
                                [ StatAssign
                                    StAssignStatement
                                      { stAssignVarName = "a",
                                        stAssignExpr = SExprL (LNum 5)
                                      }
                                ]
                            }
                      ]
                  }
              ),
            ""
          ),
          ( -- nested if-elseif-else inside an if clause
            "if (amount > msg.value / 2 ) { \
            \ if (amount > msg.value / 2 ) {a=3;} \
            \ else if (amount > msg.value) { a=4;} \
            \ else {a=5;} } \
            \ else if (amount > msg.value) { a=4;} \
            \ else {a=5;}",
            -- a=3 belongs to the if scope, but a=4 not
            Right
              ( IfStatement
                  { stIfCond =
                      SExprB
                        ( ExprBinary
                            { leftOperand = SExprVar "amount",
                              rightOperand =
                                SExprB
                                  ( ExprBinary
                                      { leftOperand = SExprS (ExprSelection {selectionBase = SExprVar "msg", selectionField = "value"}),
                                        rightOperand = SExprL (LNum 2),
                                        bOperator = ArithmeticDivision
                                      }
                                  ),
                              bOperator = ComparisionMore
                            }
                        ),
                    stIfThen =
                      [ StatIf
                          IfStatement
                            { stIfCond =
                                SExprB
                                  ExprBinary
                                    { leftOperand = SExprVar "amount",
                                      rightOperand =
                                        SExprB
                                          ExprBinary
                                            { leftOperand =
                                                SExprS
                                                  ExprSelection
                                                    { selectionBase = SExprVar "msg",
                                                      selectionField = "value"
                                                    },
                                              rightOperand = SExprL (LNum 2),
                                              bOperator = ArithmeticDivision
                                            },
                                      bOperator = ComparisionMore
                                    },
                              stIfThen =
                                [ StatAssign
                                    StAssignStatement
                                      { stAssignVarName = "a",
                                        stAssignExpr = SExprL (LNum 3)
                                      }
                                ],
                              stIfElse =
                                [ StatIf
                                    IfStatement
                                      { stIfCond =
                                          SExprB
                                            ExprBinary
                                              { leftOperand = SExprVar "amount",
                                                rightOperand =
                                                  SExprS
                                                    ExprSelection
                                                      { selectionBase = SExprVar "msg",
                                                        selectionField = "value"
                                                      },
                                                bOperator = ComparisionMore
                                              },
                                        stIfThen =
                                          [ StatAssign
                                              StAssignStatement
                                                { stAssignVarName = "a",
                                                  stAssignExpr = SExprL (LNum 4)
                                                }
                                          ],
                                        stIfElse =
                                          [ StatAssign
                                              StAssignStatement
                                                { stAssignVarName = "a",
                                                  stAssignExpr = SExprL (LNum 5)
                                                }
                                          ]
                                      }
                                ]
                            }
                      ],
                    stIfElse =
                      [ StatIf
                          IfStatement
                            { stIfCond =
                                SExprB
                                  ExprBinary
                                    { leftOperand = SExprVar "amount",
                                      rightOperand =
                                        SExprS
                                          ExprSelection
                                            { selectionBase = SExprVar "msg",
                                              selectionField = "value"
                                            },
                                      bOperator = ComparisionMore
                                    },
                              stIfThen =
                                [ StatAssign
                                    StAssignStatement
                                      { stAssignVarName = "a",
                                        stAssignExpr = SExprL (LNum 4)
                                      }
                                ],
                              stIfElse =
                                [ StatAssign
                                    StAssignStatement
                                      { stAssignVarName = "a",
                                        stAssignExpr = SExprL (LNum 5)
                                      }
                                ]
                            }
                      ]
                  }
              ),
            ""
          ),
          ( "if (amount > msg.value / 2 ) {a=3;} elseif (amount > msg.value) { a=4;} else {a=5;}",
            Left ["\"i\"", "\"i\"", "\"i\"", "space", "space", "\"{\""],
            "if (amount > msg.value / 2 ) {a=3;} elseif (amount > msg.value) { a=4;} else {a=5;}"
          )
        ]
  forM_ testCases $ verifyParser "if statement" pIfStatement

parseForStatementSpec :: Spec
parseForStatementSpec = do
  let testCases =
        -- todo: support the following cases:
        -- for(int i=0;i<10;i++){ a++; }
        [ ( "for(int i=0;i<10;i+=1){ a=1; break }",
            Right
              ( ForStatement
                  { forDecl = Just (StVarDefStatement {stVarType = STypeInt 256, stVarName = "i", stVarLocation = Storage, stVarExpr = Just (SExprL (LNum 0)), stVarComment = Nothing}),
                    forExprStat = Just (SExprB (ExprBinary {leftOperand = SExprVar "i", rightOperand = SExprL (LNum 10), bOperator = ComparisionLess})),
                    forCond = Just (SExprB (ExprBinary {leftOperand = SExprVar "i", rightOperand = SExprL (LNum 1), bOperator = CompoundAddition})),
                    forBody =
                      [ StatAssign
                          StAssignStatement
                            { stAssignVarName = "a",
                              stAssignExpr = SExprL (LNum 1)
                            },
                        StatBreak
                      ]
                  }
              ),
            ""
          ),
          ( "for(;i<10;i+=1){ a=1; continue }",
            Right
              ( ForStatement
                  { forDecl = Nothing,
                    forExprStat = Just (SExprB (ExprBinary {leftOperand = SExprVar "i", rightOperand = SExprL (LNum 10), bOperator = ComparisionLess})),
                    forCond = Just (SExprB (ExprBinary {leftOperand = SExprVar "i", rightOperand = SExprL (LNum 1), bOperator = CompoundAddition})),
                    forBody =
                      [ StatAssign
                          StAssignStatement
                            { stAssignVarName = "a",
                              stAssignExpr = SExprL (LNum 1)
                            },
                        StatContinue
                      ]
                  }
              ),
            ""
          ),
          ( "for(int i=0;;i+=1){ a=1; return; }",
            Right
              ( ForStatement
                  { forDecl = Just (StVarDefStatement {stVarType = STypeInt 256, stVarName = "i", stVarLocation = Storage, stVarExpr = Just (SExprL (LNum 0)), stVarComment = Nothing}),
                    forExprStat = Nothing,
                    forCond = Just (SExprB (ExprBinary {leftOperand = SExprVar "i", rightOperand = SExprL (LNum 1), bOperator = CompoundAddition})),
                    forBody =
                      [ StatAssign
                          StAssignStatement
                            { stAssignVarName = "a",
                              stAssignExpr = SExprL (LNum 1)
                            },
                        StatReturn Nothing
                      ]
                  }
              ),
            ""
          ),
          ( "for(int i=0;i<10;){ a=1; return 123;}",
            Right
              ( ForStatement
                  { forDecl = Just (StVarDefStatement {stVarType = STypeInt 256, stVarName = "i", stVarLocation = Storage, stVarExpr = Just (SExprL (LNum 0)), stVarComment = Nothing}),
                    forExprStat = Just (SExprB (ExprBinary {leftOperand = SExprVar "i", rightOperand = SExprL (LNum 10), bOperator = ComparisionLess})),
                    forCond = Nothing,
                    forBody =
                      [ StatAssign
                          StAssignStatement
                            { stAssignVarName = "a",
                              stAssignExpr = SExprL (LNum 1)
                            },
                        StatReturn (Just (SExprL (LNum 123)))
                      ]
                  }
              ),
            ""
          ),
          ( "for(;;){ a=1; }",
            Right
              ( ForStatement
                  { forDecl = Nothing,
                    forExprStat = Nothing,
                    forCond = Nothing,
                    forBody =
                      [ StatAssign
                          StAssignStatement
                            { stAssignVarName = "a",
                              stAssignExpr = SExprL (LNum 1)
                            }
                      ]
                  }
              ),
            ""
          )
        ]
  forM_ testCases $ verifyParser "for statement" pForStatement

parseWhileStatementSpec :: Spec
parseWhileStatementSpec = do
  let testCases =
        [ ( "while(true){a=1;}",
            Right
              ( WhileStatement
                  { whileCond = SExprL (LBool True),
                    whileBody = [StatAssign (StAssignStatement {stAssignVarName = "a", stAssignExpr = SExprL (LNum 1)})]
                  }
              ),
            ""
          ),
          ( "while(false){a=1;}",
            Right
              ( WhileStatement
                  { whileCond = SExprL (LBool False),
                    whileBody = [StatAssign (StAssignStatement {stAssignVarName = "a", stAssignExpr = SExprL (LNum 1)})]
                  }
              ),
            ""
          ),
          ( "while(i<10){a=1;}",
            Right
              ( WhileStatement
                  { whileCond = SExprB (ExprBinary {leftOperand = SExprVar "i", rightOperand = SExprL (LNum 10), bOperator = ComparisionLess}),
                    whileBody = [StatAssign (StAssignStatement {stAssignVarName = "a", stAssignExpr = SExprL (LNum 1)})]
                  }
              ),
            ""
          )
        ]
  forM_ testCases $ verifyParser "while statement" pWhileStatement

parseDoWhileStatementSpec :: Spec
parseDoWhileStatementSpec = do
  let testCases =
        [ ( "do {a=1;} while(true);",
            Right
              ( DoWhileStatement
                  { doWhileCond = SExprL (LBool True),
                    doWhileBody = [StatAssign (StAssignStatement {stAssignVarName = "a", stAssignExpr = SExprL (LNum 1)})]
                  }
              ),
            ""
          ),
          ( "do {a=1;} while(false);",
            Right
              ( DoWhileStatement
                  { doWhileCond = SExprL (LBool False),
                    doWhileBody = [StatAssign (StAssignStatement {stAssignVarName = "a", stAssignExpr = SExprL (LNum 1)})]
                  }
              ),
            ""
          ),
          ( "do {a=1;} while(i<10);",
            Right
              ( DoWhileStatement
                  { doWhileCond = SExprB (ExprBinary {leftOperand = SExprVar "i", rightOperand = SExprL (LNum 10), bOperator = ComparisionLess}),
                    doWhileBody = [StatAssign (StAssignStatement {stAssignVarName = "a", stAssignExpr = SExprL (LNum 1)})]
                  }
              ),
            ""
          )
        ]
  forM_ testCases $ verifyParser "do-while statement" pDoWhileStatement

parseTryCatchStatementSpec :: Spec
parseTryCatchStatementSpec = do
  let testCases =
        [ ( "try Foo(_owner) returns (Foo foo) { a = 1; } catch (bytes memory reason) { a = 3 ;}",
            Right
              ( TryStatement
                  { tryExpr = SExprF (ExprFnCall {fnContractName = Nothing, fnName = "Foo", fnArguments = FnCallArgsList [SExprVar "_owner"]}),
                    tryReturns = [FnDeclArg {fnArgTp = STypeCustom "Foo", fnArgName = Just "foo", fnArgLocation = Storage}],
                    tryBody = [StatAssign (StAssignStatement {stAssignVarName = "a", stAssignExpr = SExprL (LNum 1)})],
                    tryCatches =
                      [ CatchStatement
                          { catchIdent = Nothing,
                            catchParams = [FnDeclArg {fnArgTp = STypeBytes 1, fnArgName = Just "reason", fnArgLocation = Memory}],
                            catchBody = [StatAssign (StAssignStatement {stAssignVarName = "a", stAssignExpr = SExprL (LNum 3)})]
                          }
                      ]
                  }
              ),
            ""
          )
        ]
  forM_ testCases $ verifyParser "try-catch statement" pTryStatement

parseCatchStatementSpec :: Spec
parseCatchStatementSpec = do
  let testCases =
        [ ( "catch (bytes memory reason) { a = 3 ;}",
            Right
              ( CatchStatement
                  { catchIdent = Nothing,
                    catchParams = [FnDeclArg {fnArgTp = STypeBytes 1, fnArgName = Just "reason", fnArgLocation = Memory}],
                    catchBody = [StatAssign (StAssignStatement {stAssignVarName = "a", stAssignExpr = SExprL (LNum 3)})]
                  }
              ),
            ""
          ),
          ( "catch Error(string memory reason) { a = 4; }",
            Right
              ( CatchStatement
                  { catchIdent = Just "Error",
                    catchParams = [FnDeclArg {fnArgTp = STypeString, fnArgName = Just "reason", fnArgLocation = Memory}],
                    catchBody = [StatAssign (StAssignStatement {stAssignVarName = "a", stAssignExpr = SExprL (LNum 4)})]
                  }
              ),
            ""
          )
        ]
  forM_ testCases $ verifyParser "catch statement only" pCatchStatment

parseEmitStatementSpec :: Spec
parseEmitStatementSpec = do
  let testCases =
        [ ( "emit Hello(1,2,3)",
            Right
              ( EmitStatement
                  { emitEventIdent = "Hello",
                    emitCallArgs = FnCallArgsList [SExprL (LNum 1), SExprL (LNum 2), SExprL (LNum 3)]
                  }
              ),
            ""
          ),
          ( "emit Deposit(\"deposit\",10000)",
            Right
              ( EmitStatement
                  { emitEventIdent = "Deposit",
                    emitCallArgs = FnCallArgsList [SExprL (LString "deposit"), SExprL (LNum 10000)]
                  }
              ),
            ""
          )
        ]
  forM_ testCases $ verifyParser "catch statement only" pEmitStatement

parseRevertStatementSpec :: Spec
parseRevertStatementSpec = do
  let testCases =
        [ ( "revert Hello(1,2,3)",
            Right
              ( RevertStatement
                  { revertEventIdent = "Hello",
                    revertCallArgs = FnCallArgsList [SExprL (LNum 1), SExprL (LNum 2), SExprL (LNum 3)]
                  }
              ),
            ""
          ),
          ( "revert Deposit(\"deposit\",10000)",
            Right
              ( RevertStatement
                  { revertEventIdent = "Deposit",
                    revertCallArgs = FnCallArgsList [SExprL (LString "deposit"), SExprL (LNum 10000)]
                  }
              ),
            ""
          )
        ]
  forM_ testCases $ verifyParser "catch statement only" pRevertStatement
