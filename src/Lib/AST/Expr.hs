{-# LANGUAGE OverloadedStrings #-}

module Lib.AST.Expr
  ( pExpression,
    pFnCallArgs,
    pFuncCallArgsFormatList,
    pElemIndex,
    pExprTenary,
    pFuncCall,
    pSelection,
  )
where

import Control.Applicative (Applicative (liftA2), optional)
import Control.Monad (guard)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State (MonadState (get, put), guard)
import Data.Char (isSpace)
import Data.Functor (($>))
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace (trace)
import Lib.AST.Model
  ( ExprBinary (ExprBinary),
    ExprFnCall (..),
    ExprIndex (ExprIndex, elemBase, elemIndex),
    ExprSelection (ExprSelection, selectionBase, selectionField),
    ExprTernary (..),
    ExprUnary (..),
    FnCallArgs (..),
    Literal (..),
    Operator (Minus),
    SExpr
      ( SExprB,
        SExprD,
        SExprF,
        SExprI,
        SExprL,
        SExprN,
        SExprParentheses,
        SExprS,
        SExprT,
        SExprU,
        SExprVar
      ),
    colon,
    leftCurlyBrace,
    leftParenthesis,
    leftSquareBracket,
    rightCurlyBrace,
    rightParenthesis,
    rightSquareBracket,
  )
import Lib.AST.Oper
  ( allOperators,
    pOpRank,
    pOpRankLast,
    pOperator,
  )
import Lib.Parser
  ( Parser,
    pBool,
    pIdentifier,
    pMany1Spaces,
    pManySpaces,
    pNumber,
    pOneKeyword,
    pString,
  )
import Text.Parsec
  ( getInput,
    many,
    many1,
    optionMaybe,
    setInput,
    try,
    (<|>),
  )

pExpression :: Parser SExpr
pExpression =
  foldr
    ( \pOp pExpr -> do
        left <- pExpr
        rest <- many ((,) <$> try (pOp <* pManySpaces) <*> try pExpr <* pManySpaces)
        return $
          foldl
            ( \acc (op, right) ->
                SExprB $ ExprBinary acc right op
            )
            left
            rest
    )
    pBasicExpr
    (pOpRankLast (concat allOperators) : fmap pOpRank allOperators)

pBasicExpr :: Parser SExpr
pBasicExpr =
  try pParenthesizedExpr
    <|> SExprN <$> try pNewCall
    -- we decide to keep supporting the unary expression during parse stage,
    -- such as '123 + -x', where we will report error during syntax check
    <|> SExprU <$> try pUnaryExpr
    <|> SExprF <$> try pFuncCall
    -- parse a function call first, if not parse it as a selection
    <|> try pSelection
    -- parse elem index after selection to solve a[x].y
    <|> try pElemIndex
    <|> SExprD <$> try pDeleteExpr
    <|> SExprT <$> try pExprTenary
    <|> SExprL <$> try pLiteral
    <|> SExprVar <$> try pIdentifier

pParenthesizedExpr :: Parser SExpr
pParenthesizedExpr = do
  SExprParentheses
    <$> ( pManySpaces
            >> pOneKeyword leftParenthesis
            >> pExpression
              <* (pManySpaces >> pOneKeyword rightParenthesis)
        )

pNewCall :: Parser ExprFnCall
pNewCall = pManySpaces >> pOneKeyword "new" >> pFuncCall

pDeleteExpr :: Parser Text
pDeleteExpr =
  pManySpaces
    >> pOneKeyword "delete"
    >> pMany1Spaces
    >> pIdentifier

pSelection :: Parser SExpr
pSelection = do
  base <-
    pManySpaces
      -- don't use pExpression, beacuse selection cases are limited
      >> ( SExprF <$> try pFuncCall
             <|> try pElemIndex
             <|> SExprVar <$> pIdentifier
         )

  fields <- many1 $ pOneKeyword "." >> pIdentifier
  return $
    foldl
      (\acc v -> SExprS $ ExprSelection {selectionBase = acc, selectionField = v})
      base
      fields

pLiteral :: Parser Literal
pLiteral =
  do
    LNum <$> try pNumber
    <|> LBool <$> try pBool
    <|> LString <$> try pString

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
    pManySpaces
      >> ( pParenthesizedExpr -- it's allowed to have space after the unary operator
             <|> SExprL <$> pLiteral
             <|> SExprVar <$> pIdentifier
             <|> SExprU <$> pUnaryExpr -- double unary expression is possible, such as '- - 1'
         )
  return
    ExprUnary
      { uOperand = operand,
        uOperator = op
      }

pFnCallArgs :: Parser FnCallArgs
pFnCallArgs =
  try pFuncCallArgsFormatNamedParameters
    <|> try pFuncCallArgsFormatList

pFuncCall :: Parser ExprFnCall
pFuncCall = do
  (ct, fName) <-
    pManySpaces
      >> liftA2
        (,)
        -- use try here to make sure the identifier and keyword are consumed in a batch
        (optionMaybe $ try $ pIdentifier <* pOneKeyword ".")
        pIdentifier
  args <- pManySpaces *> pFnCallArgs
  return
    ExprFnCall
      { fnContractName = ct,
        fnName = fName,
        fnArguments = args
      }

pFuncCallNamedParameterKeyValue :: Parser (Text, SExpr)
pFuncCallNamedParameterKeyValue =
  liftA2
    (,)
    ( pIdentifier
        <* pManySpaces
        <* pOneKeyword ":"
        <* pManySpaces
    )
    pExpression

pFuncCallArgsFormatNamedParameters :: Parser FnCallArgs
pFuncCallArgsFormatNamedParameters = do
  arg1 <-
    pOneKeyword leftParenthesis
      >> pManySpaces
      >> pOneKeyword leftCurlyBrace
      >> optionMaybe pFuncCallNamedParameterKeyValue
  args <-
    many
      ( pManySpaces
          >> pOneKeyword "," -- todo: this matching should be unified in a function in parser
          >> pManySpaces
          >> try pFuncCallNamedParameterKeyValue
      )
  _ <-
    pOneKeyword rightCurlyBrace
      >> pManySpaces
      >> pOneKeyword rightParenthesis
  return $ FnCallArgsNamedParameters $ maybeToList arg1 ++ args

-- todo: refine me with sepBy
pFuncCallArgsFormatList :: Parser FnCallArgs
pFuncCallArgsFormatList = do
  arg1 <-
    pOneKeyword leftParenthesis
      >> optionMaybe pExpression
  args <-
    many $
      pManySpaces
        >> pOneKeyword ","
        >> pManySpaces
        >> pExpression
  _ <- pOneKeyword rightParenthesis
  return $ FnCallArgsList $ maybeToList arg1 ++ args

pElemIndex :: Parser SExpr
pElemIndex = do
  elem <- SExprVar <$> (pManySpaces >> pIdentifier)
  idxs <- many1 $ pOneKeyword leftSquareBracket >> pExpression <* pOneKeyword rightSquareBracket
  return $ foldl (\acc idx -> SExprI $ ExprIndex {elemBase = acc, elemIndex = idx}) elem idxs

pExprTenary :: Parser ExprTernary
pExprTenary = do
  s <- getInput
  let (cond, s') = T.break (== '?') s -- break the expression first to parse them one by one
  guard $ s' /= ""
  setInput cond
  cond <-
    pManySpaces
      >> pExpression
        <* pManySpaces
  leftS <- getInput
  guard $ leftS == ""
  setInput s' >> pManySpaces >> pOneKeyword "?"

  left <-
    pManySpaces
      >> pExpression
        <* (pManySpaces >> pOneKeyword colon)
  right <- pManySpaces >> pExpression
  return
    ExprTernary
      { ternaryCond = cond,
        leftTernaryExpr = left,
        rightTernaryExpr = right
      }
