{-# LANGUAGE OverloadedStrings #-}

module Lib.AST.ExprSpec (spec) where

import Control.Monad (forM_)
import Lib.AST.Expr
  ( pElemIndex,
    pExprTenary,
    pExpression,
    pFuncCall,
    pSelection,
  )
import Lib.AST.Model
  ( DataLocation (Calldata, Memory, Storage),
    ExprBinary (ExprBinary, bOperator, leftOperand, rightOperand),
    ExprFnCall (ExprFnCall, fnArguments, fnContractName, fnName),
    ExprIndex (ExprIndex, elemBase, elemIndex),
    ExprSelection (ExprSelection, selectionBase, selectionField),
    ExprTernary
      ( ExprTernary,
        leftTernaryExpr,
        rightTernaryExpr,
        ternaryCond
      ),
    ExprUnary (ExprUnary, uOperand, uOperator),
    FnCallArgs (FnCallArgsList, FnCallArgsNamedParameters),
    Literal (LBool, LNum),
    Operator (ArithmeticAddition, ArithmeticDivision, ArithmeticExp, ArithmeticMultiplication, BitAnd, BitExor, BitNeg, BitOr, ComparisonLess, ComparisonLessEqual, ComparisonMore, ComparisonMoreEqual, LogicalAnd, LogicalEqual, LogicalInequal, LogicalNegation, LogicalOr, Minus, ShiftLeft, ShiftRight),
    SExpr (SExprB, SExprD, SExprF, SExprI, SExprL, SExprN, SExprParentheses, SExprS, SExprT, SExprU, SExprVar),
  )
import Lib.AST.Util
import Lib.TestCommon (exactlyParserVerifier)
import Test.Hspec (Spec)

spec :: Spec
spec = do
  parseLogicalExpressionSpec
  parseArithmeticExpressionSpec
  parseBitExpressionSpec
  parseComparisonExpressionSpec
  parseShiftExpressionSpec
  parseUnaryExpressionSpec
  parseSelectionExprSpec
  parseFuncCallSpec
  parseElemIndexSpec
  parseLocationModifierSpec
  parseTernaryExprSpec
  parseDeleteNewExprSpec
  parsePrecedenceExprSpec

parsePrecedenceExprSpec :: Spec
parsePrecedenceExprSpec = do
  let testCases =
        [ ( "true?false:\ntrue \n|| true \n&& false == 3 < false + 3 + 2*m[1][2]**2",
            Right
              ( SExprT
                  ExprTernary
                    { ternaryCond = SExprL (LBool True),
                      leftTernaryExpr = SExprL (LBool False),
                      rightTernaryExpr =
                        SExprB
                          ExprBinary
                            { leftOperand = SExprL (LBool True),
                              rightOperand =
                                SExprB
                                  ExprBinary
                                    { -- 3 < false + 3 + 2*m[1][2]**2++
                                      leftOperand = SExprL (LBool True),
                                      rightOperand =
                                        SExprB
                                          ExprBinary
                                            { leftOperand = SExprL (LBool False),
                                              rightOperand =
                                                SExprB
                                                  ExprBinary
                                                    { leftOperand = SExprL (LNum 3),
                                                      rightOperand =
                                                        SExprB
                                                          ExprBinary
                                                            { leftOperand =
                                                                SExprB
                                                                  ExprBinary
                                                                    { leftOperand = SExprL (LBool False),
                                                                      rightOperand = SExprL (LNum 3),
                                                                      bOperator = ArithmeticAddition
                                                                    },
                                                              rightOperand =
                                                                SExprB
                                                                  ExprBinary
                                                                    { -- 2*

                                                                      leftOperand = SExprL (LNum 2),
                                                                      rightOperand =
                                                                        SExprB
                                                                          ExprBinary
                                                                            { leftOperand =
                                                                                SExprI
                                                                                  ExprIndex
                                                                                    { elemBase =
                                                                                        SExprI
                                                                                          ExprIndex
                                                                                            { elemBase = SExprVar "m",
                                                                                              elemIndex = SExprL (LNum 1)
                                                                                            },
                                                                                      elemIndex = SExprL (LNum 2)
                                                                                    },
                                                                              rightOperand = SExprL (LNum 2),
                                                                              bOperator = ArithmeticExp
                                                                            },
                                                                      bOperator = ArithmeticMultiplication
                                                                    },
                                                              bOperator = ArithmeticAddition
                                                            },
                                                      bOperator = ComparisonLess
                                                    },
                                              bOperator = LogicalEqual
                                            },
                                      bOperator = LogicalAnd
                                    },
                              bOperator = LogicalOr
                            }
                    }
              ),
            ""
          ),
          ( "2*m[1][2]\n **2",
            Right
              ( SExprB
                  ExprBinary
                    { leftOperand = SExprL (LNum 2),
                      rightOperand =
                        SExprB
                          ExprBinary
                            { leftOperand =
                                SExprI
                                  ExprIndex
                                    { elemBase =
                                        SExprI
                                          ExprIndex
                                            { elemBase = SExprVar "m",
                                              elemIndex = SExprL (LNum 1)
                                            },
                                      elemIndex = SExprL (LNum 2)
                                    },
                              rightOperand = SExprL (LNum 2),
                              bOperator = ArithmeticExp
                            },
                      bOperator = ArithmeticMultiplication
                    }
              ),
            ""
          )
        ]
  forM_ testCases $ exactlyParserVerifier "unary expression" pExpression

parseDeleteNewExprSpec :: Spec
parseDeleteNewExprSpec = do
  let testCases =
        [ ( "new Contract(123)",
            Right
              ( SExprN
                  ExprFnCall
                    { fnContractName = Nothing,
                      fnName = "Contract",
                      fnArguments = FnCallArgsList [SExprL (LNum 123)]
                    }
              ),
            ""
          ),
          ( "delete var",
            Right (SExprD "var"),
            ""
          ),
          ( "delete \nvar",
            Right (SExprD "var"),
            ""
          )
        ]
  forM_ testCases $ exactlyParserVerifier "unary expression" pExpression

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
          ( "true ? \n1*2 : 0",
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
          ( "x&&y \n? (1+4*5) : 0",
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
          ( "true ? false ? 1 :2 : \n0",
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
  forM_ testCases $ exactlyParserVerifier "variable definition" pExprTenary

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
            Left ["\"w\"", "\"w\"", "\"w\"", "\"memory\"", "\"storage\"", "\"calldata\""],
            "wrong"
          )
        ]
  forM_ testCases $ exactlyParserVerifier "variable definition" pLocationModifier

parseSelectionExprSpec :: Spec
parseSelectionExprSpec = do
  -- some test case has an invalid syntax but still could be parsed in AST
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
  forM_ testCases $ exactlyParserVerifier "selection expression" pSelection

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
          ( "- 1",
            Right $
              SExprU $
                ExprUnary
                  { uOperand = SExprL $ LNum 1,
                    uOperator = Minus
                  },
            ""
          ),
          ( "- - 1",
            Right
              ( SExprU
                  ExprUnary
                    { uOperator = Minus,
                      uOperand =
                        SExprU
                          ExprUnary
                            { uOperator = Minus,
                              uOperand = SExprL (LNum 1)
                            }
                    }
              ),
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
                      -- this is intended, and in the syntax stage we can easily recognize such case
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
  forM_ testCases $ exactlyParserVerifier "unary expression" pExpression

parseShiftExpressionSpec :: Spec
parseShiftExpressionSpec = do
  -- some test case has an invalid syntax but still could be parsed in AST
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
          ( "1>>2 <<3",
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
          ( "1>> (2<<3)",
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
  forM_ testCases $ exactlyParserVerifier "shift expression" pExpression

parseComparisonExpressionSpec :: Spec
parseComparisonExpressionSpec = do
  -- some test case has an invalid syntax but still could be parsed in AST
  let testCases =
        [ ( "1 <= 2",
            Right $
              SExprB $
                ExprBinary
                  { leftOperand = SExprL $ LNum 1,
                    rightOperand = SExprL $ LNum 2,
                    bOperator = ComparisonLessEqual
                  },
            ""
          ),
          ( "1<2",
            Right $
              SExprB $
                ExprBinary
                  { leftOperand = SExprL $ LNum 1,
                    rightOperand = SExprL $ LNum 2,
                    bOperator = ComparisonLess
                  },
            ""
          ),
          ( "1 > 2",
            Right $
              SExprB $
                ExprBinary
                  { leftOperand = SExprL $ LNum 1,
                    rightOperand = SExprL $ LNum 2,
                    bOperator = ComparisonMore
                  },
            ""
          ),
          ( "1>=2",
            Right $
              SExprB $
                ExprBinary
                  { leftOperand = SExprL $ LNum 1,
                    rightOperand = SExprL $ LNum 2,
                    bOperator = ComparisonMoreEqual
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
                              bOperator = ComparisonMoreEqual
                            },
                      rightOperand = SExprL (LNum 3),
                      bOperator = ComparisonLessEqual
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
                                bOperator = ComparisonMore
                              },
                      bOperator = ComparisonLess
                    }
              ),
            ""
          )
        ]
  forM_ testCases $ exactlyParserVerifier "Comparison expression" pExpression

parseBitExpressionSpec :: Spec
parseBitExpressionSpec = do
  -- some test case has an invalid syntax but still could be parsed in AST
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
          ( "1&2~3", -- negation takes a higher precedence
            Right
              ( SExprB
                  ExprBinary
                    { leftOperand = SExprL (LNum 1),
                      rightOperand =
                        SExprB
                          ExprBinary
                            { leftOperand = SExprL (LNum 2),
                              rightOperand = SExprL (LNum 3),
                              bOperator = BitNeg
                            },
                      bOperator = BitAnd
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
  forM_ testCases $ exactlyParserVerifier "bit expression" pExpression

parseArithmeticExpressionSpec :: Spec
parseArithmeticExpressionSpec = do
  -- some test case has an invalid syntax but still could be parsed in AST
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
  forM_ testCases $ exactlyParserVerifier "arithmetic expression" pExpression

parseLogicalExpressionSpec :: Spec
parseLogicalExpressionSpec = do
  -- some test case has an invalid syntax but still could be parsed in AST
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
          ( -- '!=' takes a higher precedence, and then is the '||'
            "123 || false != arg1",
            Right
              ( SExprB
                  ExprBinary
                    { leftOperand = SExprL (LNum 123),
                      rightOperand =
                        SExprB
                          ExprBinary
                            { leftOperand = SExprL (LBool False),
                              rightOperand = SExprVar "arg1",
                              bOperator = LogicalInequal
                            },
                      bOperator = LogicalOr
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
  forM_ testCases $ exactlyParserVerifier "logical expression" pExpression

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
          ( "uint(2)", -- todo: should we separate the type cast from FunctionDefinition call?
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
  forM_ testCases $ exactlyParserVerifier "FunctionDefinition call" pFuncCall

parseElemIndexSpec :: Spec
parseElemIndexSpec = do
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
  forM_ testCases $ exactlyParserVerifier "index retrieving" pElemIndex
