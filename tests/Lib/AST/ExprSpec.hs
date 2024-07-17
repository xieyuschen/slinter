module Lib.AST.ExprSpec (spec) where

import Control.Monad (forM_)
import Lib.AST.Expr
import Lib.AST.Model
import Lib.TestCommon
import Test.Hspec

spec :: Spec
spec = do
  parseLogcicalExpressionSpec
  parseArithemeticExpressionSpec
  parseOperatorSpec
  parseBitExpressionSpec
  parseComparisionExpressionSpec
  parseShiftExpressionSpec
  parseUnaryExpressionSpec
  parseSelectionExprSepc
  parseFuncCallSpec
  parsepElemIndexSpec
  parseLocationModifierSpec
  parseTernaryExprSpec

parseTernaryExprSpec :: Spec
parseTernaryExprSpec = do
  let testCases =
        [ ( "true ? 1 : 0",
            Right
              ExprTernary
                { ternaryCond = SExprL (LBool True),
                  leftTernaryExpr = SExprL (LNum 1),
                  rightTernaryExpr = SExprL (LNum 0)
                },
            ""
          ),
          ( "true ? 1*2 : 0",
            Right
              ExprTernary
                { ternaryCond = SExprL (LBool True),
                  leftTernaryExpr =
                    SExprB
                      ExprBinary
                        { leftOperand = SExprL (LNum 1),
                          rightOperand = SExprL (LNum 2),
                          bOperator = ArithmeticMultiplication
                        },
                  rightTernaryExpr = SExprL (LNum 0)
                },
            ""
          ),
          ( "x&&y ? (1+4*5) : 0",
            Right
              ExprTernary
                { ternaryCond =
                    SExprB
                      ExprBinary
                        { leftOperand = SExprVar "x",
                          rightOperand = SExprVar "y",
                          bOperator = LogicalAnd
                        },
                  leftTernaryExpr =
                    SExprParentheses
                      ( SExprB
                          ExprBinary
                            { leftOperand = SExprL (LNum 1),
                              rightOperand =
                                SExprB
                                  ExprBinary
                                    { leftOperand = SExprL (LNum 4),
                                      rightOperand = SExprL (LNum 5),
                                      bOperator = ArithmeticMultiplication
                                    },
                              bOperator = ArithmeticAddition
                            }
                      ),
                  rightTernaryExpr = SExprL (LNum 0)
                },
            ""
          ),
          ( "true ? false ? 1 :2 : 0",
            Right
              ExprTernary
                { ternaryCond = SExprL (LBool True),
                  leftTernaryExpr =
                    SExprT
                      ExprTernary
                        { ternaryCond = SExprL (LBool False),
                          leftTernaryExpr = SExprL (LNum 1),
                          rightTernaryExpr = SExprL (LNum 2)
                        },
                  rightTernaryExpr = SExprL (LNum 0)
                },
            ""
          )
        ]
  forM_ testCases $ verifyParser "variable definition" pExprTenary

parseLocationModifierSpec :: Spec
parseLocationModifierSpec = do
  let testCases =
        [ ( "memory",
            Right Memory,
            ""
          ),
          ( "storage",
            Right Storage,
            ""
          ),
          ( "calldata",
            Right Calldata,
            ""
          ),
          ( "wrong",
            Left "fail to find desired charactor: 'memory';fail to find desired charactor: 'storage';fail to find desired charactor: 'calldata';",
            "wrong"
          )
        ]
  forM_ testCases $ verifyParser "variable definition" pLocationModifer

parseSelectionExprSepc :: Spec
parseSelectionExprSepc = do
  -- some test caes has an invalid syntax but still could be parsed in AST
  let testCases =
        [ ( "a.b",
            Right $
              SExprS $
                ExprSelection
                  { selectionBase = SExprVar "a",
                    selectionField = "b"
                  },
            ""
          ),
          ( "a.b.c",
            Right $
              SExprS $
                ExprSelection
                  { selectionBase =
                      SExprS $
                        ExprSelection
                          { selectionBase = SExprVar "a",
                            selectionField = "b"
                          },
                    selectionField = "c"
                  },
            ""
          ),
          ( "a().b",
            Right $
              SExprS $
                ExprSelection
                  { selectionBase =
                      SExprF
                        ExprFnCall
                          { fnContractName = Nothing,
                            fnName = "a",
                            fnArguments = FnCallArgsList []
                          },
                    selectionField = "b"
                  },
            ""
          ),
          ( "ctc.a().b",
            Right $
              SExprS $
                ExprSelection
                  { selectionBase =
                      SExprF
                        ExprFnCall
                          { fnContractName = Just "ctc",
                            fnName = "a",
                            fnArguments = FnCallArgsList []
                          },
                    selectionField = "b"
                  },
            ""
          )
        ]
  forM_ testCases $ verifyParser "selection expression" pSelection

parseUnaryExpressionSpec :: Spec
parseUnaryExpressionSpec = do
  let testCases =
        [ ( "-1",
            Right $
              SExprU $
                ExprUnary
                  { uOperand = SExprL $ LNum 1,
                    uOperator = Minus
                  },
            ""
          ),
          ( "-x",
            Right $
              SExprU $
                ExprUnary
                  { uOperand = SExprVar "x",
                    uOperator = Minus
                  },
            ""
          ),
          ( "-(1)",
            Right
              ( SExprU
                  ExprUnary
                    { uOperand = SExprParentheses (SExprL (LNum 1)),
                      uOperator = Minus
                    }
              ),
            ""
          ),
          ( "-(-1)",
            Right
              ( SExprU
                  ExprUnary
                    { uOperator = Minus,
                      uOperand =
                        SExprParentheses
                          ( SExprU
                              ExprUnary
                                { uOperand = SExprL (LNum 1),
                                  uOperator = Minus
                                }
                          )
                    }
              ),
            ""
          ),
          ( "-(-(x))",
            Right
              ( SExprU
                  ExprUnary
                    { uOperator = Minus,
                      uOperand =
                        SExprParentheses
                          ( SExprU
                              ExprUnary
                                { uOperand = SExprParentheses (SExprVar "x"),
                                  uOperator = Minus
                                }
                          )
                    }
              ),
            ""
          ),
          ( "-(-(x-1))",
            Right
              ( SExprU
                  ExprUnary
                    { uOperator = Minus,
                      uOperand =
                        SExprParentheses
                          ( SExprU
                              ExprUnary
                                { uOperator = Minus,
                                  uOperand =
                                    SExprParentheses
                                      ( SExprB
                                          ExprBinary
                                            { leftOperand = SExprVar "x",
                                              rightOperand = SExprL (LNum 1),
                                              bOperator = Minus
                                            }
                                      )
                                }
                          )
                    }
              ),
            ""
          ),
          ( "-(-(x-1) && 234)",
            Right
              ( SExprU
                  ExprUnary
                    { uOperator = Minus,
                      uOperand =
                        SExprParentheses
                          ( SExprB
                              ExprBinary
                                { leftOperand =
                                    SExprU
                                      ExprUnary
                                        { uOperator = Minus,
                                          uOperand =
                                            SExprParentheses
                                              ( SExprB
                                                  ExprBinary
                                                    { leftOperand = SExprVar "x",
                                                      rightOperand = SExprL (LNum 1),
                                                      bOperator = Minus
                                                    }
                                              )
                                        },
                                  rightOperand = SExprL (LNum 234),
                                  bOperator = LogicalAnd
                                }
                          )
                    }
              ),
            ""
          ),
          ( "-x + 123",
            Right $
              SExprB $
                ExprBinary
                  { leftOperand =
                      SExprU $
                        ExprUnary
                          { uOperand = SExprVar "x",
                            uOperator = Minus
                          },
                    rightOperand = SExprL $ LNum 123,
                    bOperator = ArithmeticAddition
                  },
            ""
          ),
          ( "123 + -x",
            Right $
              SExprB $
                ExprBinary
                  { leftOperand = SExprL $ LNum 123,
                    rightOperand =
                      -- this is intended, and in the syntax stage we can easily recognize such csae
                      SExprU $
                        ExprUnary
                          { uOperand = SExprVar "x",
                            uOperator = Minus
                          },
                    bOperator = ArithmeticAddition
                  },
            ""
          )
        ]
  forM_ testCases $ verifyParser "unary expression" pExpression

parseShiftExpressionSpec :: Spec
parseShiftExpressionSpec = do
  -- some test caes has an invalid syntax but still could be parsed in AST
  let testCases =
        [ ( "1 << 2",
            Right $
              SExprB $
                ExprBinary
                  { leftOperand = SExprL $ LNum 1,
                    rightOperand = SExprL $ LNum 2,
                    bOperator = ShiftLeft
                  },
            ""
          ),
          ( "1 >> 2",
            Right $
              SExprB $
                ExprBinary
                  { leftOperand = SExprL $ LNum 1,
                    rightOperand = SExprL $ LNum 2,
                    bOperator = ShiftRight
                  },
            ""
          ),
          ( "1>>2<<3",
            Right
              ( SExprB
                  ExprBinary
                    { leftOperand =
                        SExprB
                          ExprBinary
                            { leftOperand = SExprL (LNum 1),
                              rightOperand = SExprL (LNum 2),
                              bOperator = ShiftRight
                            },
                      rightOperand = SExprL (LNum 3),
                      bOperator = ShiftLeft
                    }
              ),
            ""
          ),
          ( "1>>(2<<3)",
            Right
              ( SExprB
                  ExprBinary
                    { leftOperand = SExprL (LNum 1),
                      rightOperand =
                        SExprParentheses $
                          SExprB
                            ExprBinary
                              { leftOperand = SExprL (LNum 2),
                                rightOperand = SExprL (LNum 3),
                                bOperator = ShiftLeft
                              },
                      bOperator = ShiftRight
                    }
              ),
            ""
          )
        ]
  forM_ testCases $ verifyParser "shift expression" pExpression

parseComparisionExpressionSpec :: Spec
parseComparisionExpressionSpec = do
  -- some test caes has an invalid syntax but still could be parsed in AST
  let testCases =
        [ ( "1 <= 2",
            Right $
              SExprB $
                ExprBinary
                  { leftOperand = SExprL $ LNum 1,
                    rightOperand = SExprL $ LNum 2,
                    bOperator = ComparisionLessEqual
                  },
            ""
          ),
          ( "1<2",
            Right $
              SExprB $
                ExprBinary
                  { leftOperand = SExprL $ LNum 1,
                    rightOperand = SExprL $ LNum 2,
                    bOperator = ComparisionLess
                  },
            ""
          ),
          ( "1 > 2",
            Right $
              SExprB $
                ExprBinary
                  { leftOperand = SExprL $ LNum 1,
                    rightOperand = SExprL $ LNum 2,
                    bOperator = ComparisionMore
                  },
            ""
          ),
          ( "1>=2",
            Right $
              SExprB $
                ExprBinary
                  { leftOperand = SExprL $ LNum 1,
                    rightOperand = SExprL $ LNum 2,
                    bOperator = ComparisionMoreEqual
                  },
            ""
          ),
          ( "1>=2<=3",
            Right
              ( SExprB
                  ExprBinary
                    { leftOperand =
                        SExprB
                          ExprBinary
                            { leftOperand = SExprL (LNum 1),
                              rightOperand = SExprL (LNum 2),
                              bOperator = ComparisionMoreEqual
                            },
                      rightOperand = SExprL (LNum 3),
                      bOperator = ComparisionLessEqual
                    }
              ),
            ""
          ),
          ( "1<(2>3)",
            Right
              ( SExprB
                  ExprBinary
                    { leftOperand = SExprL (LNum 1),
                      rightOperand =
                        SExprParentheses $
                          SExprB
                            ExprBinary
                              { leftOperand = SExprL (LNum 2),
                                rightOperand = SExprL (LNum 3),
                                bOperator = ComparisionMore
                              },
                      bOperator = ComparisionLess
                    }
              ),
            ""
          )
        ]
  forM_ testCases $ verifyParser "comparision expression" pExpression

parseBitExpressionSpec :: Spec
parseBitExpressionSpec = do
  -- some test caes has an invalid syntax but still could be parsed in AST
  let testCases =
        [ ( "1 & 2",
            Right $
              SExprB $
                ExprBinary
                  { leftOperand = SExprL $ LNum 1,
                    rightOperand = SExprL $ LNum 2,
                    bOperator = BitAnd
                  },
            ""
          ),
          ( "1|2",
            Right $
              SExprB $
                ExprBinary
                  { leftOperand = SExprL $ LNum 1,
                    rightOperand = SExprL $ LNum 2,
                    bOperator = BitOr
                  },
            ""
          ),
          ( "1^ 2",
            Right $
              SExprB $
                ExprBinary
                  { leftOperand = SExprL $ LNum 1,
                    rightOperand = SExprL $ LNum 2,
                    bOperator = BitExor
                  },
            ""
          ),
          ( "1~2",
            Right $
              SExprB $
                ExprBinary
                  { leftOperand = SExprL $ LNum 1,
                    rightOperand = SExprL $ LNum 2,
                    bOperator = BitNeg
                  },
            ""
          ),
          ( "1&2~3",
            Right
              ( SExprB
                  ExprBinary
                    { leftOperand =
                        SExprB
                          ExprBinary
                            { leftOperand = SExprL (LNum 1),
                              rightOperand = SExprL (LNum 2),
                              bOperator = BitAnd
                            },
                      rightOperand = SExprL (LNum 3),
                      bOperator = BitNeg
                    }
              ),
            ""
          ),
          ( "1~(2^3)",
            Right
              ( SExprB
                  ExprBinary
                    { leftOperand = SExprL (LNum 1),
                      rightOperand =
                        SExprParentheses $
                          SExprB
                            ExprBinary
                              { leftOperand = SExprL (LNum 2),
                                rightOperand = SExprL (LNum 3),
                                bOperator = BitExor
                              },
                      bOperator = BitNeg
                    }
              ),
            ""
          )
        ]
  forM_ testCases $ verifyParser "bit expression" pExpression

parseArithemeticExpressionSpec :: Spec
parseArithemeticExpressionSpec = do
  -- some test caes has an invalid syntax but still could be parsed in AST
  let testCases =
        [ ( "1",
            Right $ SExprL $ LNum 1,
            ""
          ),
          ( "1+2",
            Right $
              SExprB $
                ExprBinary
                  { leftOperand = SExprL $ LNum 1,
                    rightOperand = SExprL $ LNum 2,
                    bOperator = ArithmeticAddition
                  },
            ""
          ),
          ( "1+2-3",
            Right
              ( SExprB
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
              ),
            ""
          ),
          ( "1+2*3",
            Right
              ( SExprB
                  ExprBinary
                    { leftOperand = SExprL (LNum 1),
                      rightOperand =
                        SExprB
                          ExprBinary
                            { leftOperand = SExprL (LNum 2),
                              rightOperand = SExprL (LNum 3),
                              bOperator = ArithmeticMultiplication
                            },
                      bOperator = ArithmeticAddition
                    }
              ),
            ""
          ),
          ( "1*2-3",
            Right
              ( SExprB
                  ExprBinary
                    { leftOperand =
                        SExprB
                          ExprBinary
                            { leftOperand = SExprL (LNum 1),
                              rightOperand = SExprL (LNum 2),
                              bOperator = ArithmeticMultiplication
                            },
                      rightOperand = SExprL (LNum 3),
                      bOperator = Minus
                    }
              ),
            ""
          ),
          ( "1*2*3-4/5/6",
            Right
              ( SExprB
                  ExprBinary
                    { leftOperand =
                        SExprB
                          ExprBinary
                            { leftOperand =
                                SExprB
                                  ExprBinary
                                    { leftOperand = SExprL (LNum 1),
                                      rightOperand = SExprL (LNum 2),
                                      bOperator = ArithmeticMultiplication
                                    },
                              rightOperand = SExprL (LNum 3),
                              bOperator = ArithmeticMultiplication
                            },
                      rightOperand =
                        SExprB
                          ExprBinary
                            { leftOperand =
                                SExprB
                                  ExprBinary
                                    { leftOperand = SExprL (LNum 4),
                                      rightOperand = SExprL (LNum 5),
                                      bOperator = ArithmeticDivision
                                    },
                              rightOperand = SExprL (LNum 6),
                              bOperator = ArithmeticDivision
                            },
                      bOperator = Minus
                    }
              ),
            ""
          )
        ]
  forM_ testCases $ verifyParser "arithmetic expression" pExpression

parseLogcicalExpressionSpec :: Spec
parseLogcicalExpressionSpec = do
  -- some test caes has an invalid syntax but still could be parsed in AST
  let testCases =
        [ ( "true",
            Right $ SExprL $ LBool True,
            ""
          ),
          ( "tru",
            Right $ SExprVar "tru",
            ""
          ),
          ( "true ! false",
            Right $
              SExprB $
                ExprBinary
                  { leftOperand = SExprL $ LBool True,
                    rightOperand = SExprL $ LBool False,
                    bOperator = LogicalNegation
                  },
            ""
          ),
          ( "tru && false",
            Right $
              SExprB $
                ExprBinary
                  { leftOperand = SExprVar "tru",
                    rightOperand = SExprL $ LBool False,
                    bOperator = LogicalAnd
                  },
            ""
          ),
          ( -- this is an invalid case, but we used it to test the parser
            "123 || false",
            Right $
              SExprB $
                ExprBinary
                  { leftOperand = SExprL $ LNum 123,
                    rightOperand = SExprL $ LBool False,
                    bOperator = LogicalOr
                  },
            ""
          ),
          ( -- this is an invalid case, but we used it to test the parser
            "123 || false != arg1",
            Right
              ( SExprB
                  ExprBinary
                    { leftOperand =
                        SExprB
                          ExprBinary
                            { leftOperand = SExprL (LNum 123),
                              rightOperand = SExprL (LBool False),
                              bOperator = LogicalOr
                            },
                      rightOperand = SExprVar "arg1",
                      bOperator = LogicalInequal
                    }
              ),
            ""
          ),
          ( "(true)",
            Right (SExprParentheses (SExprL (LBool True))),
            ""
          ),
          ( "(true && false)",
            Right
              ( SExprParentheses
                  ( SExprB
                      ExprBinary
                        { leftOperand = SExprL (LBool True),
                          rightOperand = SExprL (LBool False),
                          bOperator = LogicalAnd
                        }
                  )
              ),
            ""
          ),
          ( "(true) || false",
            Right
              ( SExprB
                  ExprBinary
                    { leftOperand = SExprParentheses (SExprL (LBool True)),
                      rightOperand = SExprL (LBool False),
                      bOperator = LogicalOr
                    }
              ),
            ""
          ),
          ( "(true && (false || true))",
            Right
              ( SExprParentheses
                  ( SExprB
                      ExprBinary
                        { leftOperand = SExprL (LBool True),
                          rightOperand =
                            SExprParentheses
                              ( SExprB
                                  ExprBinary
                                    { leftOperand = SExprL (LBool False),
                                      rightOperand = SExprL (LBool True),
                                      bOperator = LogicalOr
                                    }
                              ),
                          bOperator = LogicalAnd
                        }
                  )
              ),
            ""
          ),
          ( "((true || false) && true)",
            Right
              ( SExprParentheses
                  ( SExprB
                      ExprBinary
                        { leftOperand =
                            SExprParentheses
                              ( SExprB
                                  ExprBinary
                                    { leftOperand = SExprL (LBool True),
                                      rightOperand = SExprL (LBool False),
                                      bOperator = LogicalOr
                                    }
                              ),
                          rightOperand = SExprL (LBool True),
                          bOperator = LogicalAnd
                        }
                  )
              ),
            ""
          )
        ]
  forM_ testCases $ verifyParser "logical expression" pExpression

parseOperatorSpec :: Spec
parseOperatorSpec = do
  let testCases =
        [ ( "&&",
            Right LogicalAnd,
            ""
          ),
          ( "||",
            Right LogicalOr,
            ""
          ),
          ( "!=",
            Right LogicalInequal,
            ""
          ),
          ( "==",
            Right LogicalEqual,
            ""
          ),
          ( "!",
            Right LogicalNegation,
            ""
          ),
          ( "+-",
            Right ArithmeticAddition,
            "-"
          ),
          ( "+",
            Right ArithmeticAddition,
            ""
          ),
          ( "-",
            Right Minus,
            ""
          ),
          ( "*",
            Right ArithmeticMultiplication,
            ""
          ),
          ( "/",
            Right ArithmeticDivision,
            ""
          ),
          ( "%",
            Right ArithmeticModulus,
            ""
          ),
          ( "**",
            Right ArithmeticExp,
            ""
          ),
          ( "<=",
            Right ComparisionLessEqual,
            ""
          ),
          ( "<",
            Right ComparisionLess,
            ""
          ),
          ( ">=",
            Right ComparisionMoreEqual,
            ""
          ),
          ( ">=",
            Right ComparisionMoreEqual,
            ""
          ),
          ( ">",
            Right ComparisionMore,
            ""
          ),
          ( ">",
            Right ComparisionMore,
            ""
          ),
          ( "&",
            Right BitAnd,
            ""
          ),
          ( "|",
            Right BitOr,
            ""
          ),
          ( "^",
            Right BitExor,
            ""
          ),
          ( "~",
            Right BitNeg,
            ""
          ),
          ( "<<",
            Right ShiftLeft,
            ""
          ),
          ( ">>",
            Right ShiftRight,
            ""
          ),
          ( "+=",
            Right CompoundAddition,
            ""
          ),
          ( "-=",
            Right CompoundMinus,
            ""
          ),
          ( "*=",
            Right CompoundMultiply,
            ""
          ),
          ( "/=",
            Right CompoundDevision,
            ""
          ),
          ( "%=",
            Right CompoundModulus,
            ""
          ),
          ( "&=",
            Right CompoundAnd,
            ""
          ),
          ( "|=",
            Right CompoundOr,
            ""
          ),
          ( "^=",
            Right CompoundExor,
            ""
          ),
          ( "<<=",
            Right CompoundLeftShift,
            ""
          ),
          ( ">>=",
            Right CompoundRightShift,
            ""
          ),
          ( "++",
            Right Increment,
            ""
          ),
          ( "--",
            Right Decrement,
            ""
          )
        ]

  forM_ testCases $ verifyParser "arithmetic expression" pOperator
  -- in this turn, we add some suffix after the operator to make sure the parse works well
  forM_ testCases $ verifyParser "arithmetic expression" pOperator . appendSuffix "1_suffix"

parseFuncCallSpec :: Spec
parseFuncCallSpec = do
  let testCases =
        [ ( "set({value: 2, key: 3})",
            Right $
              ExprFnCall
                { fnContractName = Nothing,
                  fnName = "set",
                  fnArguments = FnCallArgsNamedParameters [("value", SExprL (LNum 2)), ("key", SExprL (LNum 3))]
                },
            ""
          ),
          ( "example.set(2, 3)",
            Right
              ExprFnCall
                { fnContractName = Just "example",
                  fnName = "set",
                  fnArguments = FnCallArgsList [SExprL (LNum 2), SExprL (LNum 3)]
                },
            ""
          ),
          ( "uint(2)", -- todo: should we seperate the type cast from function call?
            Right
              ExprFnCall
                { fnContractName = Nothing,
                  fnName = "uint",
                  fnArguments = FnCallArgsList [SExprL (LNum 2)]
                },
            ""
          ),
          ( "example.set({value: 2+1, key: -3})",
            Right
              ExprFnCall
                { fnContractName = Just "example",
                  fnName = "set",
                  fnArguments =
                    FnCallArgsNamedParameters
                      [ ( "value",
                          SExprB
                            ExprBinary
                              { leftOperand = SExprL (LNum 2),
                                rightOperand = SExprL (LNum 1),
                                bOperator = ArithmeticAddition
                              }
                        ),
                        ( "key",
                          SExprU
                            ExprUnary
                              { uOperator = Minus,
                                uOperand = SExprL (LNum 3)
                              }
                        )
                      ]
                },
            ""
          ),
          ( "set()",
            Right $
              ExprFnCall
                { fnContractName = Nothing,
                  fnName = "set",
                  fnArguments = FnCallArgsList []
                },
            ""
          ),
          ( "set({})",
            Right $
              ExprFnCall
                { fnContractName = Nothing,
                  fnName = "set",
                  fnArguments = FnCallArgsNamedParameters []
                },
            ""
          )
        ]
  forM_ testCases $ verifyParser "function call" pFuncCall

parsepElemIndexSpec :: Spec
parsepElemIndexSpec = do
  let testCases =
        [ ( "m[1]",
            Right
              ( SExprI
                  ExprIndex
                    { elemBase = SExprVar "m",
                      elemIndex = SExprL (LNum 1)
                    }
              ),
            ""
          ),
          ( "m[1+1]",
            Right
              ( SExprI
                  ExprIndex
                    { elemBase = SExprVar "m",
                      elemIndex =
                        SExprB
                          ExprBinary
                            { leftOperand = SExprL (LNum 1),
                              rightOperand = SExprL (LNum 1),
                              bOperator = ArithmeticAddition
                            }
                    }
              ),
            ""
          ),
          ( "m[1+x]",
            Right
              ( SExprI
                  ExprIndex
                    { elemBase = SExprVar "m",
                      elemIndex =
                        SExprB
                          ExprBinary
                            { leftOperand = SExprL (LNum 1),
                              rightOperand = SExprVar "x",
                              bOperator = ArithmeticAddition
                            }
                    }
              ),
            ""
          ),
          ( "m[1+2*3]",
            Right
              ( SExprI
                  ExprIndex
                    { elemBase = SExprVar "m",
                      elemIndex =
                        SExprB
                          ExprBinary
                            { leftOperand = SExprL (LNum 1),
                              rightOperand =
                                SExprB
                                  ExprBinary
                                    { leftOperand = SExprL (LNum 2),
                                      rightOperand = SExprL (LNum 3),
                                      bOperator = ArithmeticMultiplication
                                    },
                              bOperator = ArithmeticAddition
                            }
                    }
              ),
            ""
          ),
          ( "m[n[3]]",
            Right
              ( SExprI
                  ExprIndex
                    { elemBase = SExprVar "m",
                      elemIndex =
                        SExprI
                          ExprIndex
                            { elemBase = SExprVar "n",
                              elemIndex = SExprL (LNum 3)
                            }
                    }
              ),
            ""
          ),
          ( "m[1][2]",
            Right
              ( SExprI
                  ExprIndex
                    { elemBase =
                        SExprI
                          ExprIndex
                            { elemBase = SExprVar "m",
                              elemIndex = SExprL (LNum 1)
                            },
                      elemIndex = SExprL (LNum 2)
                    }
              ),
            ""
          )
        ]
  forM_ testCases $ verifyParser "index retrieveing" pElemIndex
