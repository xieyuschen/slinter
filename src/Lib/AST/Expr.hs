module Lib.AST.Expr where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (guard)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State (MonadState (get, put))
import Data.Char (isSpace)
import Lib.AST.Model
  ( Exprv (Exprv, leftOperand, operator, rightOperand),
    Literal (..),
    Operator (..),
    SExpression (..),
  )
import Lib.Parser
  ( Parser,
    pBool,
    pIdentifier,
    pManySpaces,
    pNumber,
    pOpt,
    pString,
  )

pExpression :: Parser SExpression
pExpression = do
  left <-
    pManySpaces
      >> ( ExprL <$> pLiteral
             <|> ExprVar <$> pIdentifier
         )
  _ <- pManySpaces
  maybeOp <- pOpt pOperator
  case maybeOp of
    Nothing -> return left
    Just op -> do
      maybeRight <- pOpt pExpression
      case maybeRight of
        Nothing -> throwError "missing a right operand"
        Just right ->
          return $
            Expr $
              Exprv
                { leftOperand = left,
                  rightOperand = right,
                  operator = op
                }

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
  case take 1 s of
    "!" -> do
      -- consume more to see whether it's a '!' or '!='
      if (length s == 1) || isSpace (last $ take 2 s)
        then do
          put $ drop 1 s
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
