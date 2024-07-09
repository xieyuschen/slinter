module Lib.AST.ExprSpec (spec) where

import Control.Monad (forM_)
import Lib.AST.Expr (pExpression, pOperator)
import Lib.AST.Model
import Lib.Parser
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  parseLogcicalExpressionSpec
  parseArithemeticExpressionSpec
  parseOperatorSpec

verifyExpression :: (String, Either ErrMsg SExpression, String) -> Spec
verifyExpression (input, expectedResult, expectedState) = do
  describe ("parse expression correctly: " ++ input) $ do
    let (result, s) = runParser pExpression input
    it "could parse the expression" $ do
      result `shouldBe` expectedResult
    it "should leave correct state" $ do
      s `shouldBe` expectedState

parseArithemeticExpressionSpec :: Spec
parseArithemeticExpressionSpec = do
  -- some test caes has an invalid syntax but still could be parsed in AST
  let testCases =
        [ ( "1",
            Right $ ExprL $ LNum 1,
            ""
          ),
          ( "1+2",
            Right $
              Expr $
                Exprv
                  { leftOperand = ExprL $ LNum 1,
                    rightOperand = ExprL $ LNum 2,
                    operator = ArithmeticAddition
                  },
            ""
          ),
          ( "1+2-3",
            Right
              ( Expr
                  Exprv
                    { leftOperand =
                        Expr
                          Exprv
                            { leftOperand = ExprL (LNum 1),
                              rightOperand = ExprL (LNum 2),
                              operator = ArithmeticAddition
                            },
                      rightOperand = ExprL (LNum 3),
                      operator = ArithmeticSubtraction
                    }
              ),
            ""
          ),
          ( "1+2*3",
            Right
              ( Expr
                  Exprv
                    { leftOperand = ExprL (LNum 1),
                      rightOperand =
                        Expr
                          Exprv
                            { leftOperand = ExprL (LNum 2),
                              rightOperand = ExprL (LNum 3),
                              operator = ArithmeticMultiplication
                            },
                      operator = ArithmeticAddition
                    }
              ),
            ""
          ),
          ( "1*2-3",
            Right
              ( Expr
                  Exprv
                    { leftOperand =
                        Expr
                          Exprv
                            { leftOperand = ExprL (LNum 1),
                              rightOperand = ExprL (LNum 2),
                              operator = ArithmeticMultiplication
                            },
                      rightOperand = ExprL (LNum 3),
                      operator = ArithmeticSubtraction
                    }
              ),
            ""
          ),
          ( "1*2*3-4/5/6",
            Right
              ( Expr
                  Exprv
                    { leftOperand =
                        Expr
                          Exprv
                            { leftOperand =
                                Expr
                                  Exprv
                                    { leftOperand = ExprL (LNum 1),
                                      rightOperand = ExprL (LNum 2),
                                      operator = ArithmeticMultiplication
                                    },
                              rightOperand = ExprL (LNum 3),
                              operator = ArithmeticMultiplication
                            },
                      rightOperand =
                        Expr
                          Exprv
                            { leftOperand =
                                Expr
                                  Exprv
                                    { leftOperand = ExprL (LNum 4),
                                      rightOperand = ExprL (LNum 5),
                                      operator = ArithmeticDivision
                                    },
                              rightOperand = ExprL (LNum 6),
                              operator = ArithmeticDivision
                            },
                      operator = ArithmeticSubtraction
                    }
              ),
            ""
          )
        ]
  forM_ testCases verifyExpression

parseLogcicalExpressionSpec :: Spec
parseLogcicalExpressionSpec = do
  -- some test caes has an invalid syntax but still could be parsed in AST
  let testCases =
        [ ( "true",
            Right $ ExprL $ LBool True,
            ""
          ),
          ( "tru",
            Right $ ExprVar "tru",
            ""
          ),
          ( "true ! false",
            Right $
              Expr $
                Exprv
                  { leftOperand = ExprL $ LBool True,
                    rightOperand = ExprL $ LBool False,
                    operator = LogicalNegation
                  },
            ""
          ),
          ( "tru && false",
            Right $
              Expr $
                Exprv
                  { leftOperand = ExprVar "tru",
                    rightOperand = ExprL $ LBool False,
                    operator = LogicalAnd
                  },
            ""
          ),
          ( -- this is an invalid case, but we used it to test the parser
            "123 || false",
            Right $
              Expr $
                Exprv
                  { leftOperand = ExprL $ LNum 123,
                    rightOperand = ExprL $ LBool False,
                    operator = LogicalOr
                  },
            ""
          ),
          ( -- this is an invalid case, but we used it to test the parser
            "123 || false != arg1",
            Right
              ( Expr
                  Exprv
                    { leftOperand =
                        Expr
                          Exprv
                            { leftOperand = ExprL (LNum 123),
                              rightOperand = ExprL (LBool False),
                              operator = LogicalOr
                            },
                      rightOperand = ExprVar "arg1",
                      operator = LogicalInequal
                    }
              ),
            ""
          ),
          ( "(true)",
            Right (ParenthesizedExpr (ExprL (LBool True))),
            ""
          ),
          ( "(true && false)",
            Right
              ( ParenthesizedExpr
                  ( Expr
                      Exprv
                        { leftOperand = ExprL (LBool True),
                          rightOperand = ExprL (LBool False),
                          operator = LogicalAnd
                        }
                  )
              ),
            ""
          ),
          ( "(true) || false",
            Right
              ( Expr
                  Exprv
                    { leftOperand = ParenthesizedExpr (ExprL (LBool True)),
                      rightOperand = ExprL (LBool False),
                      operator = LogicalOr
                    }
              ),
            ""
          ),
          ( "(true && (false || true))",
            Right
              ( ParenthesizedExpr
                  ( Expr
                      Exprv
                        { leftOperand = ExprL (LBool True),
                          rightOperand =
                            ParenthesizedExpr
                              ( Expr
                                  Exprv
                                    { leftOperand = ExprL (LBool False),
                                      rightOperand = ExprL (LBool True),
                                      operator = LogicalOr
                                    }
                              ),
                          operator = LogicalAnd
                        }
                  )
              ),
            ""
          ),
          ( "((true || false) && true)",
            Right
              ( ParenthesizedExpr
                  ( Expr
                      Exprv
                        { leftOperand =
                            ParenthesizedExpr
                              ( Expr
                                  Exprv
                                    { leftOperand = ExprL (LBool True),
                                      rightOperand = ExprL (LBool False),
                                      operator = LogicalOr
                                    }
                              ),
                          rightOperand = ExprL (LBool True),
                          operator = LogicalAnd
                        }
                  )
              ),
            ""
          )
        ]
  forM_ testCases verifyExpression

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
          )
        ]
  forM_ testCases $ \(input, expectedResult, expectedState) -> do
    describe ("parse function modifiers: " ++ input) $ do
      let (result, s) = runParser pOperator input
      it "could parse the modifiers" $ do
        result `shouldBe` expectedResult
      it "should leave correct state" $ do
        s `shouldBe` expectedState
