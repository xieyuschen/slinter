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

parseVarDefinitionSpec :: Spec
parseVarDefinitionSpec = do
  let testCases =
        [ ( "uint256 public name = hello;",
            Left ["\"n\"", "\"n\"", "space", "\"=\""],
            "uint256 public name = hello;"
          ),
          ( "fixed hello=2345;",
            Right
              StVarDefinition
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
              StVarDefinition
                { stVarType = STypeFixed 128 18,
                  stVarName = "hello",
                  stVarLocation = Memory,
                  stVarExpr = Just (SExprL (LNum 2345)),
                  stVarComment = Nothing
                },
            ""
          )
        ]
  forM_ testCases $ verifyParser "variable definition" pStVarDefinition

parseStateVarSpec :: Spec
parseStateVarSpec = do
  let testCases =
        [ ( "uint256 public name;",
            Right
              StateVariable
                { svVisibleSpecifier = VsPublic,
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
                { svVisibleSpecifier = VsPublic,
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
                { svVisibleSpecifier = VsPublic,
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
                { svVisibleSpecifier = VsInternal,
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
              StAssign
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
              StAssign
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
              StAssign
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
              StAssign
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
              ( StstIfElse
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
                          ( StAssign
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
              ( StstIfElse
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
                          ( StAssign
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
              ( StstIfElse
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
                          ( StAssign
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
              ( StstIfElse
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
                          ( StAssign
                              { stAssignVarName = "a",
                                stAssignExpr = SExprL (LNum 3)
                              }
                          ),
                        StatAssign
                          StAssign
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
              ( StstIfElse
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
                          ( StAssign
                              { stAssignVarName = "a",
                                stAssignExpr = SExprL (LNum 3)
                              }
                          )
                      ],
                    stIfElse =
                      [ StatAssign
                          StAssign
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
              ( StstIfElse
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
                          ( StAssign
                              { stAssignVarName = "a",
                                stAssignExpr = SExprL (LNum 3)
                              }
                          )
                      ],
                    stIfElse =
                      [ StatIfElse
                          StstIfElse
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
                                    StAssign
                                      { stAssignVarName = "a",
                                        stAssignExpr = SExprL (LNum 4)
                                      }
                                ],
                              stIfElse =
                                [ StatAssign
                                    StAssign
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
              ( StstIfElse
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
                      [ StatIfElse
                          StstIfElse
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
                                    StAssign
                                      { stAssignVarName = "a",
                                        stAssignExpr = SExprL (LNum 3)
                                      }
                                ],
                              stIfElse =
                                [ StatIfElse
                                    StstIfElse
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
                                              StAssign
                                                { stAssignVarName = "a",
                                                  stAssignExpr = SExprL (LNum 4)
                                                }
                                          ],
                                        stIfElse =
                                          [ StatAssign
                                              StAssign
                                                { stAssignVarName = "a",
                                                  stAssignExpr = SExprL (LNum 5)
                                                }
                                          ]
                                      }
                                ]
                            }
                      ],
                    stIfElse =
                      [ StatIfElse
                          StstIfElse
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
                                    StAssign
                                      { stAssignVarName = "a",
                                        stAssignExpr = SExprL (LNum 4)
                                      }
                                ],
                              stIfElse =
                                [ StatAssign
                                    StAssign
                                      { stAssignVarName = "a",
                                        stAssignExpr = SExprL (LNum 5)
                                      }
                                ]
                            }
                      ]
                  }
              ),
            ""
          )
        ]
  forM_ testCases $ verifyParser "single if statement" pStateIfElse
