module Lib.AST.Expr where

import Control.Applicative (Alternative ((<|>)), Applicative (liftA2), liftA3, optional)
import Control.Lens
import Control.Monad (guard)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State
import Data.Char (isSpace)
import Data.Maybe (fromMaybe, maybeToList)
import Debug.Trace
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
    <|> SExprF <$> pTry pFuncCall
    -- parse a function call first, if not parse it as a selection
    <|> pTry pSelection
    -- parse elem index after selection to solve a[x].y
    <|> pTry pElemIndex
    <|> SExprT <$> pTry pExprTenary
    <|> SExprL <$> pLiteral
    <|> SExprVar <$> pIdentifier

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

isOperatorEnd :: String -> Bool
isOperatorEnd s = (length s == 1) || isSpace (last $ take 2 s)

-- isFollowed check whether the want string is the second charactor of s
-- it requires the string at least has 2 charactor otherwise it throws an exception
isSecond :: String -> String -> Bool
isSecond s want = maybe False (== head want) (s ^? element 1)

isTriple :: String -> String -> Bool
isTriple s want = maybe False (== head want) (s ^? element 2)

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

pOperator3Char :: Parser Operator
pOperator3Char = do
  s <- get
  guard $ length s >= 3 -- at least 3 characters
  put $ drop 3 s
  case take 3 s of
    ">>=" -> return CompoundRightShift
    "<<=" -> return CompoundLeftShift
    _ -> throwError "unsupported operator in three characters"

pOperator2Char :: Parser Operator
pOperator2Char = do
  s <- get
  guard $ length s >= 2 -- at least 2 characters
  put $ drop 2 s
  case take 2 s of
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
    _ -> throwError "unsupport operator in two characters"

pOperator1Char :: Parser Operator
pOperator1Char = do
  s <- get
  guard $ not $ null s -- check whether the string has enough chars to consume
  put $ drop 1 s
  case take 1 s of
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
    _ -> throwError "unsupport operator in one character"

pOperator :: Parser Operator
pOperator = do
  pManySpaces
    >> pTry pOperator3Char
      <|> pTry pOperator2Char
      <|> pTry pOperator1Char

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
