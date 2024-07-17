module Lib.AST.OperSpec where

import Control.Monad (forM_)
import Lib.AST.Model
import Lib.AST.Oper
import Lib.TestCommon
import Test.Hspec

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