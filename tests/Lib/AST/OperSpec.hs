{-# LANGUAGE OverloadedStrings #-}

module Lib.AST.OperSpec where

import Control.Monad (forM_)
import Lib.AST.Model
  ( Operator
      ( ArithmeticAddition,
        ArithmeticDivision,
        ArithmeticExp,
        ArithmeticModulus,
        ArithmeticMultiplication,
        BitAnd,
        BitExor,
        BitNeg,
        BitOr,
        ComparisonLess,
        ComparisonLessEqual,
        ComparisonMore,
        ComparisonMoreEqual,
        CompoundAddition,
        CompoundAnd,
        CompoundDivision,
        CompoundExor,
        CompoundLeftShift,
        CompoundMinus,
        CompoundModulus,
        CompoundMultiply,
        CompoundOr,
        CompoundRightShift,
        Decrement,
        Increment,
        LogicalAnd,
        LogicalEqual,
        LogicalInequal,
        LogicalNegation,
        LogicalOr,
        Minus,
        ShiftLeft,
        ShiftRight
      ),
  )
import Lib.AST.Oper (pOperator)
import Lib.TestCommon (appendSuffix, exactlyParserVerifier)
import Test.Hspec (Spec)

spec :: Spec
spec = do
  parseOperatorSpec

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
            Right ComparisonLessEqual,
            ""
          ),
          ( "<",
            Right ComparisonLess,
            ""
          ),
          ( ">=",
            Right ComparisonMoreEqual,
            ""
          ),
          ( ">=",
            Right ComparisonMoreEqual,
            ""
          ),
          ( ">",
            Right ComparisonMore,
            ""
          ),
          ( ">",
            Right ComparisonMore,
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
            Right CompoundDivision,
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

  forM_ testCases $ exactlyParserVerifier "arithmetic expression" pOperator
  -- in this turn, we add some suffix after the operator to make sure the parse works well
  forM_ testCases $
    exactlyParserVerifier
      "arithmetic expression with add-on"
      pOperator
      . appendSuffix "1_suffix"
