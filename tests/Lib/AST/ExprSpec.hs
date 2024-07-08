module Lib.AST.ExprSpec (spec) where

import Control.Monad (forM_)
import Lib.AST.Expr (pExpression, pOperator)
import Lib.AST.Model
import Lib.Parser
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  parseExpressionSpec
  parseOperatorSpec

parseExpressionSpec :: Spec
parseExpressionSpec = do
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
            Right $
              Expr $
                Exprv
                  { leftOperand = ExprL $ LNum 123,
                    rightOperand =
                      Expr $
                        Exprv
                          { leftOperand = ExprL $ LBool False,
                            rightOperand = ExprVar "arg1",
                            operator = LogicalInequal
                          },
                    operator = LogicalOr
                  },
            ""
          )
        ]
  forM_ testCases $ \(input, expectedResult, expectedState) -> do
    describe ("parse function modifiers: " ++ input) $ do
      let (result, s) = runParser pExpression input
      it "could parse the modifiers" $ do
        result `shouldBe` expectedResult
      it "should leave correct state" $ do
        s `shouldBe` expectedState

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
            Left "unsupported operator",
            "+-"
          ),
          ( "!+",
            Left "unsupported operator",
            "!+"
          )
        ]
  forM_ testCases $ \(input, expectedResult, expectedState) -> do
    describe ("parse function modifiers: " ++ input) $ do
      let (result, s) = runParser pOperator input
      it "could parse the modifiers" $ do
        result `shouldBe` expectedResult
      it "should leave correct state" $ do
        s `shouldBe` expectedState
