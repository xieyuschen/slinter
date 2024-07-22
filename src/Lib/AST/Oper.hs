{-# LANGUAGE OverloadedStrings #-}

module Lib.AST.Oper where

import Control.Monad.State (guard)
import Data.Text (Text)
import qualified Data.Text as T
import Lib.AST.Model (Operator (..))
import Lib.Parser (Parser, pManySpaces)
import Text.Parsec

pOperator3Char :: Parser Operator
pOperator3Char = do
  s <- getInput
  guard $ T.length s >= 3 -- at least 3 characters
  setInput $ T.drop 3 s
  case T.take 3 s of
    ">>=" -> return CompoundRightShift
    "<<=" -> return CompoundLeftShift
    _ -> fail "unsupported operator in three characters"

pOperator2Char :: Parser Operator
pOperator2Char = do
  s <- getInput
  guard $ T.length s >= 2 -- at least 2 characters
  setInput $ T.drop 2 s
  case T.take 2 s of
    "+=" -> return CompoundAddition
    "++" -> return Increment
    "-=" -> return CompoundMinus
    "--" -> return Decrement
    "**" -> return ArithmeticExp
    "*=" -> return CompoundMultiply
    "/=" -> return CompoundDevision
    "%=" -> return CompoundModulus
    "!=" -> return LogicalInequal
    "&&" -> return LogicalAnd
    "&=" -> return CompoundAnd
    "||" -> return LogicalOr
    "|=" -> return CompoundOr
    "^=" -> return CompoundExor
    "<=" -> return ComparisionLessEqual
    "<<" -> return ShiftLeft
    ">=" -> return ComparisionMoreEqual
    ">>" -> return ShiftRight
    "==" -> return LogicalEqual
    _ -> fail "unsupport operator in two characters"

pOperator1Char :: Parser Operator
pOperator1Char = do
  s <- getInput
  guard $ not $ T.null s -- check whether the string has enough chars to consume
  setInput $ T.drop 1 s
  case T.take 1 s of
    "+" -> return ArithmeticAddition
    "-" -> return Minus
    "/" -> return ArithmeticDivision
    "*" -> return ArithmeticMultiplication
    "%" -> return ArithmeticModulus
    "!" -> return LogicalNegation
    "^" -> return BitExor
    "~" -> return BitNeg
    "&" -> return BitAnd
    "|" -> return BitOr
    "<" -> return ComparisionLess
    ">" -> return ComparisionMore
    _ -> fail "unsupport operator in one character"

pOperator :: Parser Operator
pOperator = do
  pManySpaces
    >> ( try pOperator3Char
           <|> try pOperator2Char
           <|> try pOperator1Char
       )

-- we use the same rank mentioned in the documentation to refre the precedences among different
-- operators, in the format of pOpRank{n} such as pOpRank1 and so on
-- https://docs.soliditylang.org/en/latest/types.html#order-of-precedence-of-operators

-- todo: support all the cases in rank1
-- todo: support the delete cases
-- todo: think about the unary minus precedence
opRank2 = [Increment, Decrement, LogicalNegation, BitNeg]

opRank3 = [ArithmeticExp]

opRank6 :: [Operator]
opRank6 = [ShiftLeft, ShiftRight]

opRank7 = [BitAnd]

opRank8 = [BitExor]

opRank9 = [BitOr]

opRank10 = [ComparisionLessEqual, ComparisionLess, ComparisionMoreEqual, ComparisionMore]

opRank11 = [LogicalEqual, LogicalInequal]

opRank12 = [LogicalAnd]

opRank13 = [LogicalOr]

-- todo: support op14 Ternary operator, and assignment
opRank14 =
  [ CompoundAddition,
    CompoundMinus,
    CompoundMultiply,
    CompoundDevision,
    CompoundModulus,
    CompoundAnd,
    CompoundOr,
    CompoundExor,
    CompoundLeftShift,
    CompoundRightShift
  ]

-- todo: support rank15 comma

pOpRankLast :: [Operator] -> Parser Operator
pOpRankLast ops = do
  op <- pManySpaces >> pOperator
  guard $ op `notElem` ops
  return op

opRank5 = [Minus, ArithmeticAddition]

opRank4 = [ArithmeticMultiplication, ArithmeticDivision]

pOpRank :: [Operator] -> Parser Operator
pOpRank ops = do
  op <- pManySpaces >> pOperator
  guard $ op `elem` ops
  return op
