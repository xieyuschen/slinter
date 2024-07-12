module Lib.AST.ExprSpec (spec) where

import Control.Monad (forM_)
import Lib.AST.Expr (pExpression, pOperator)
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
  parseAssignExpressionSpec

parseAssignExpressionSpec :: Spec
parseAssignExpressionSpec = do
  -- some test caes has an invalid syntax but still could be parsed in AST
  let testCases =
        [ ( "1 = 2",
            Right $
              SExprB $
                ExprBinary
                  { leftOperand = SExprL $ LNum 1,
                    rightOperand = SExprL $ LNum 2,
                    bOperator = Assign
                  },
            ""
          )
        ]
  forM_ testCases $ verifyParser "assign expression" pExpression

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
                      bOperator = ArithmeticSubtraction
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
                      bOperator = ArithmeticSubtraction
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
                      bOperator = ArithmeticSubtraction
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
            Right ArithmeticSubtraction,
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
          ( "=",
            Right Assign,
            ""
          )
        ]

  forM_ testCases $ verifyParser "arithmetic expression" pOperator
  -- in this turn, we add some suffix after the operator to make sure the parse works well
  forM_ testCases $ verifyParser "arithmetic expression" pOperator . appendSuffix "1_suffix"
