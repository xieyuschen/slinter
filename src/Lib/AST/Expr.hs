module Lib.AST.Expr where

import Control.Applicative (Alternative ((<|>)), Applicative (liftA2), optional)
import Control.Monad (guard)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State
import Data.Char (isSpace)
import Data.Maybe (fromMaybe, maybeToList)
import Lib.AST.Model
import Lib.AST.Oper
import Lib.Parser

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
        rest <- many (pManySpaces >> ((,) <$> pTry pOp <*> pExpr))
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
  pParenthesizedExpr
    -- we decide to keep supporting the unary expression during parse stage,
    -- such as '123 + -x', where we will report error during syntax check
    <|> SExprU <$> pUnaryExpr
    <|> SExprF <$> pTry pFuncCall
    -- parse a function call first, if not parse it as a selection
    <|> pTry pSelection
    -- parse elem index after selection to solve a[x].y
    <|> pTry pElemIndex
    <|> SExprT <$> pTry pExprTenary
    <|> SExprL <$> pLiteral
    <|> SExprVar <$> pIdentifier

pParenthesizedExpr :: Parser SExpr
pParenthesizedExpr = do
  SExprParentheses
    <$> ( pManySpaces
            >> pOneKeyword leftParenthesis
            >> pExpression
              <* (pManySpaces >> pOneKeyword rightParenthesis)
        )

pSelection :: Parser SExpr
pSelection = do
  base <-
    pManySpaces
      -- don't use pExpression, beacuse selection cases are limited
      >> ( SExprF <$> pTry pFuncCall
             <|> pTry pElemIndex
             <|> SExprVar <$> pIdentifier
         )

  fields <- pMany1 $ pOneKeyword "." >> pIdentifier
  return $
    foldl
      (\acc v -> SExprS $ ExprSelection {selectionBase = acc, selectionField = v})
      base
      fields

pLiteral :: Parser Literal
pLiteral =
  do
    LNum <$> pNumber
    <|> LBool <$> pBool
    <|> LString <$> pString

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
        -- use pTry here to make sure the identifier and keyword are consumed in a batch
        (optional $ pTry $ pIdentifier <* pOneKeyword ".")
        pIdentifier
  args <- pTry pFuncCallArgsNamedParameters <|> pTry pFuncCallArgsList
  return
    ExprFnCall
      { fnContractName = ct,
        fnName = fName,
        fnArguments = args
      }

pFuncCallNamedParameterKeyValue :: Parser (String, SExpr)
pFuncCallNamedParameterKeyValue =
  liftA2
    (,)
    (pManySpaces >> pIdentifier <* (pManySpaces >> pOneKeyword ":"))
    pExpression

pFuncCallArgsNamedParameters :: Parser FnCallArgs
pFuncCallArgsNamedParameters = do
  arg1 <-
    pManySpaces
      >> pOneKeyword leftParenthesis
      >> pManySpaces
      >> pOneKeyword leftCurlyBrace
      >> optional pFuncCallNamedParameterKeyValue
  args <-
    many
      ( pManySpaces
          >> pOneKeyword "," -- todo: this matching should be unified in a function in parser
          >> pManySpaces
          >> pTry pFuncCallNamedParameterKeyValue
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
      >> optional pExpression
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
  idxs <- pMany1 $ pOneKeyword leftSquareBracket >> pExpression <* pOneKeyword rightSquareBracket
  return $ foldl (\acc idx -> SExprI $ ExprIndex {elemBase = acc, elemIndex = idx}) elem idxs

pLocationModifer :: Parser DataLocation
pLocationModifer =
  pStringTo "memory" (const Memory)
    <|> pStringTo "storage" (const Storage)
    <|> pStringTo "calldata" (const Calldata)

pExprTenary :: Parser ExprTernary
pExprTenary = do
  s <- get
  let (cond, s') = break (== '?') s -- break the expression first to parse them one by one
  guard $ s' /= ""
  put cond
  cond <-
    pManySpaces
      >> pExpression
        <* pManySpaces
  leftS <- get
  guard $ leftS == ""
  put s' >> pManySpaces >> pOneKeyword "?"

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
