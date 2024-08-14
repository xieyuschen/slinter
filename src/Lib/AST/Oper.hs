{-# LANGUAGE OverloadedStrings #-}

module Lib.AST.Oper (pOperator, allOperators, pOpRank, pOpRankLast) where

import Control.Monad.State (guard)
import Data.Text (Text)
import qualified Data.Text as T
import Lib.AST.Model (Operator (..))
import Lib.Parser (Parser, pManySpaces)
import Text.Parsec (getInput, setInput, try, (<|>))

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
    "/=" -> return CompoundDivision
    "%=" -> return CompoundModulus
    "!=" -> return LogicalInequal
    "&&" -> return LogicalAnd
    "&=" -> return CompoundAnd
    "||" -> return LogicalOr
    "|=" -> return CompoundOr
    "^=" -> return CompoundExor
    "<=" -> return ComparisonLessEqual
    "<<" -> return ShiftLeft
    ">=" -> return ComparisonMoreEqual
    ">>" -> return ShiftRight
    "==" -> return LogicalEqual
    _ -> fail "un-support operator in two characters"

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
    "<" -> return ComparisonLess
    ">" -> return ComparisonMore
    _ -> fail "un-support operator in one character"

pOperator :: Parser Operator
pOperator = do
  pManySpaces
    >> ( try pOperator3Char
           <|> try pOperator2Char
           <|> try pOperator1Char
       )

allOperators :: [[Operator]]
allOperators =
  reverse
    [ opRank2,
      opRank3,
      opRank4,
      opRank5,
      opRank6,
      opRank7,
      opRank8,
      opRank9,
      opRank10,
      opRank11,
      opRank12,
      opRank13
    ]

-- we use the same rank mentioned in the documentation to refer the precedences among different
-- operators, in the format of pOpRank{n} such as pOpRank1 and so on
-- https://docs.soliditylang.org/en/latest/types.html#order-of-precedence-of-operators

-- todo: support all the cases in rank1
-- todo: support the delete cases
-- todo: think about the unary minus precedence
opRank2 :: [Operator]
opRank2 = [Increment, Decrement, LogicalNegation, BitNeg]

opRank3 :: [Operator]
opRank3 = [ArithmeticExp]

opRank6 :: [Operator]
opRank6 = [ShiftLeft, ShiftRight]

opRank7 :: [Operator]
opRank7 = [BitAnd]

opRank8 :: [Operator]
opRank8 = [BitExor]

opRank9 :: [Operator]
opRank9 = [BitOr]

opRank10 :: [Operator]
opRank10 = [ComparisonLessEqual, ComparisonLess, ComparisonMoreEqual, ComparisonMore]

opRank11 :: [Operator]
opRank11 = [LogicalEqual, LogicalInequal]

opRank12 :: [Operator]
opRank12 = [LogicalAnd]

opRank13 :: [Operator]
opRank13 = [LogicalOr]

-- todo: support op14 Ternary operator, and assignment
opRank14 :: [Operator]
opRank14 =
  [ CompoundAddition,
    CompoundMinus,
    CompoundMultiply,
    CompoundDivision,
    CompoundModulus,
    CompoundAnd,
    CompoundOr,
    CompoundExor,
    CompoundLeftShift,
    CompoundRightShift
  ]

-- todo: support rank15 comma

pOpRank :: [Operator] -> Parser Operator
pOpRank ops = do
  op <- pManySpaces >> pOperator
  guard $ op `elem` ops
  return op

pOpRankLast :: [Operator] -> Parser Operator
pOpRankLast ops = do
  op <- pManySpaces >> pOperator
  guard $ op `notElem` ops
  return op

opRank5 :: [Operator]
opRank5 = [Minus, ArithmeticAddition]

opRank4 :: [Operator]
opRank4 = [ArithmeticMultiplication, ArithmeticDivision]
