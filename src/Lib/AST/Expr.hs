module Lib.AST.Expr where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (guard)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State
import Data.Char (isSpace)
import Lib.AST.Model
import Lib.Parser

pExpression :: Parser SExpression
pExpression = do
  -- todo: make it a parser function
  left <- pTerm
  rest <- pMany (pManySpaces >> ((,) <$> pAddOp <*> pTerm))
  return $ foldl (\acc (op, right) -> Expr $ Exprv acc right op) left rest

pFactor :: Parser SExpression
pFactor = pParenthesizedExpr <|> (ExprL <$> pLiteral) <|> (ExprVar <$> pIdentifier)

pTerm :: Parser SExpression
pTerm = do
  left <- pFactor
  rest <- pMany (pManySpaces >> ((,) <$> pMulOp <*> pFactor))
  return $ foldl (\acc (op, right) -> Expr $ Exprv acc right op) left rest

pParenthesizedExpr :: Parser SExpression
pParenthesizedExpr = do
  _ <- pManySpaces >> pOne leftParenthesis id
  expr <- pExpression
  _ <- pManySpaces >> pOne rightParenthesis id
  return $ ParenthesizedExpr expr

pLiteral :: Parser Literal
pLiteral =
  do
    LNum <$> pNumber
    <|> LBool <$> pBool
    <|> LString <$> pString

pOperator :: Parser Operator
pOperator = do
  _ <- pManySpaces
  s <- get
  guard $ not $ null s -- check whether the string has enough chars to consume
  put $ drop 1 s
  case take 1 s of
    -- todo: the '+' in '+-' will be consumed, check whether we need to support it further
    "+" -> return ArithmeticAddition
    "-" -> return ArithmeticSubtraction
    "*" -> return ArithmeticMultiplication
    "/" -> return ArithmeticDivision
    "%" -> return ArithmeticModulus
    "!" -> do
      -- consume more to see whether it's a '!' or '!='
      if (length s == 1) || isSpace (last $ take 2 s)
        then do
          return LogicalNegation
        else
          if tail (take 2 s) == "="
            then do
              put $ drop 2 s
              return LogicalInequal
            else do
              throwError "unsupported operator"
    _ -> do
      put $ drop 2 s
      case take 2 s of
        "&&" -> return LogicalAnd
        "||" -> return LogicalOr
        "==" -> return LogicalEqual
        _ -> do
          put s
          throwError "unsupported operator"

pAddOp :: Parser Operator
pAddOp = do
  s <- get
  op <- pManySpaces >> pOperator
  case op of
    ArithmeticMultiplication -> do
      put s
      throwError "shouldn't use * here"
    ArithmeticDivision -> do
      put s
      throwError "shouldn't use / here"
    _ -> do
      -- for now, we only support precedence between */ and +-
      return op

pMulOp :: Parser Operator
pMulOp = do
  s <- get
  op <- pManySpaces >> pOperator
  case op of
    ArithmeticMultiplication -> return ArithmeticMultiplication
    ArithmeticDivision -> return ArithmeticDivision
    _ -> do
      put s
      throwError "not * or /"
