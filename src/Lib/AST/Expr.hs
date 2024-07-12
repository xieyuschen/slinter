module Lib.AST.Expr where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (guard)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State
import Data.Char (isSpace)
import Lib.AST.Model
import Lib.Parser

pExpression :: Parser SExpr
pExpression = do
  left <- pTerm
  rest <- many (pManySpaces >> ((,) <$> pTry pAddOp <*> pTerm))
  return $ foldl (\acc (op, right) -> SExprB $ ExprBinary acc right op) left rest

pFactor :: Parser SExpr
pFactor =
  pParenthesizedExpr
    -- we decide to keep supporting the unary expression during parse stage,
    -- such as '123 + -x', where we will report error during syntax check
    <|> SExprU <$> pUnaryExpr
    <|> (SExprL <$> pLiteral)
    <|> (SExprVar <$> pIdentifier)

pTerm :: Parser SExpr
pTerm = do
  left <- pFactor
  rest <- many (pManySpaces >> ((,) <$> pTry pMulOp <*> pFactor))
  return $ foldl (\acc (op, right) -> SExprB $ ExprBinary acc right op) left rest

pParenthesizedExpr :: Parser SExpr
pParenthesizedExpr = do
  SExprParentheses
    <$> ( pManySpaces
            >> pOneKeyword leftParenthesis
            >> pExpression
              <* (pManySpaces >> pOneKeyword rightParenthesis)
        )

pLiteral :: Parser Literal
pLiteral =
  do
    LNum <$> pNumber
    <|> LBool <$> pBool
    <|> LString <$> pString

isOperatorEnd :: String -> Bool
isOperatorEnd s = (length s == 1) || isSpace (last $ take 2 s)

-- isFollowed check whether the want string is the second charactor of s
-- it requires the string at least has 2 charactor otherwise it throws an exception
isSecond :: String -> String -> Bool
isSecond s want = tail (take 2 s) == want

isUnaryOp :: Operator -> Bool
isUnaryOp Minus = True
isUnaryOp _ = False

pUnaryExpr :: Parser ExprUnary
pUnaryExpr = do
  op <- pManySpaces >> pOperator
  guard $ isUnaryOp op
  -- don't use pExpression here because we don't want to parse the whole expression after unary
  -- for example, -(x-1) && 234, we should parse the -(x-1) as an expression only
  operand <-
    pParenthesizedExpr
      <|> SExprL <$> pLiteral
      <|> SExprVar <$> pIdentifier
  return
    ExprUnary
      { uOperand = operand,
        uOperator = op
      }

pOperator :: Parser Operator
pOperator = do
  s <- pManySpaces >> get
  guard $ not $ null s -- check whether the string has enough chars to consume
  put $ drop 1 s
  case take 1 s of
    -- todo: the '+' in '+-' will be consumed, check whether we need to support it further
    "+" -> return ArithmeticAddition
    "-" -> return Minus
    "*" -> case s of
      -- we need this branch to ensure the length of s for the following matching,
      -- so we cannot move this logic into otherwise
      _
        | isOperatorEnd s -> return ArithmeticMultiplication
        | isSecond s "*" ->
            put (drop 2 s)
              >> return ArithmeticExp
        | otherwise -> return ArithmeticMultiplication
    "/" -> return ArithmeticDivision
    "%" -> return ArithmeticModulus
    "!" -> case s of
      _
        | isOperatorEnd s -> return LogicalNegation
        | isSecond s "=" ->
            put (drop 2 s)
              >> return LogicalInequal
        | otherwise -> return LogicalNegation
    "^" -> return BitExor
    "~" -> return BitNeg
    "&" -> case s of
      _
        | isOperatorEnd s -> return BitAnd
        | isSecond s "&" ->
            put (drop 2 s)
              >> return LogicalAnd
        | otherwise -> return BitAnd
    "|" -> case s of
      _
        | isOperatorEnd s -> return BitOr
        | isSecond s "|" ->
            put (drop 2 s)
              >> return LogicalOr
        | otherwise -> return BitOr
    "<" -> case s of
      _
        | isOperatorEnd s -> return ComparisionLess
        | isSecond s "=" ->
            put (drop 2 s)
              >> return ComparisionLessEqual
        | isSecond s "<" ->
            put (drop 2 s)
              >> return ShiftLeft
        | otherwise -> return ComparisionLess
    ">" -> case s of
      _
        | isOperatorEnd s -> return ComparisionMore
        | isSecond s "=" ->
            put (drop 2 s)
              >> return ComparisionMoreEqual
        | isSecond s ">" ->
            put (drop 2 s)
              >> return ShiftRight
        | otherwise -> return ComparisionMore
    "=" -> case s of
      _
        | isSecond s "=" -> put (drop 2 s) >> return LogicalEqual
        | otherwise -> return Assign
    _ -> put s >> throwError "unsupported operator"

pAddOp :: Parser Operator
pAddOp = do
  op <- pManySpaces >> pOperator
  case op of
    ArithmeticMultiplication -> do
      throwError "shouldn't use * here"
    ArithmeticDivision -> do
      throwError "shouldn't use / here"
    _ -> do
      -- for now, we only support precedence between */ and +-
      return op

pMulOp :: Parser Operator
pMulOp = do
  op <- pManySpaces >> pOperator
  case op of
    ArithmeticMultiplication -> return ArithmeticMultiplication
    ArithmeticDivision -> return ArithmeticDivision
    _ -> do
      throwError "not * or /"