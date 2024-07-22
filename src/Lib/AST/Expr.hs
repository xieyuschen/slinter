{-# LANGUAGE OverloadedStrings #-}

module Lib.AST.Expr where

import Control.Applicative (Applicative (liftA2), optional)
import Control.Monad (guard)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State (MonadState (get, put), guard)
import Data.Char (isSpace)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace (trace)
import Lib.AST.Model
import Lib.AST.Oper
import Lib.Parser
  ( Parser,
    pBool,
    pIdentifier,
    pManySpaces,
    pNumber,
    pOneKeyword,
    pString,
  )
import Text.Parsec

ops :: [[Operator]]
ops =
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

pExpression :: Parser SExpr
pExpression =
  foldr
    ( \pOp pExpr -> do
        left <- pExpr
        rest <- many ((,) <$> try (pOp <* pManySpaces) <*> try pExpr <* pManySpaces)
        trace ("expr:" ++ show left) $ pure ()
        trace ("expr:" ++ show rest) $ pure ()
        return $
          foldl
            ( \acc (op, right) ->
                SExprB $ ExprBinary acc right op
            )
            left
            rest
    )
    pBasicExpr
    (pOpRankLast (concat ops) : fmap pOpRank ops)

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
    >> pManySpaces
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
    pParenthesizedExpr
      <|> SExprL <$> pLiteral
      <|> SExprVar <$> pIdentifier
  return
    ExprUnary
      { uOperand = operand,
        uOperator = op
      }

pFuncCall :: Parser ExprFnCall
pFuncCall = do
  (ct, fName) <-
    pManySpaces
      >> liftA2
        (,)
        -- use try here to make sure the identifier and keyword are consumed in a batch
        (optionMaybe $ try $ pIdentifier <* pOneKeyword ".")
        pIdentifier
  args <- try pFuncCallArgsNamedParameters <|> try pFuncCallArgsList
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

pFuncCallArgsNamedParameters :: Parser FnCallArgs
pFuncCallArgsNamedParameters = do
  arg1 <-
    pManySpaces
      >> pOneKeyword leftParenthesis
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

pFuncCallArgsList :: Parser FnCallArgs
pFuncCallArgsList = do
  arg1 <-
    pManySpaces
      >> pOneKeyword leftParenthesis
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

pLocationModifer :: Parser DataLocation
pLocationModifer =
  (pOneKeyword "memory" >> return Memory)
    <|> (pOneKeyword "storage" >> return Storage)
    <|> (pOneKeyword "calldata" >> return Calldata)

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
