{-# LANGUAGE OverloadedStrings #-}

module Lib.AST.Stat where

import Control.Applicative (liftA2)
import Control.Monad.Trans.Accum (look)
import Data.Maybe (fromMaybe)
import Lib.AST.Comment (pComment)
import Lib.AST.Expr (pExpression, pLocationModifer)
import Lib.AST.Model
  ( DataLocation (Storage),
    DoWhileStatement (DoWhileStatement, doWhileBody, doWhileCond),
    ForStatement (..),
    IfStatement (..),
    StAssignStatement (..),
    StVarDefStatement (..),
    Stat (..),
    StateVariable (..),
    VisibilitySpecifier (VsInternal),
    WhileStatement (..),
    leftCurlyBrace,
    leftParenthesis,
    rightCurlyBrace,
    rightParenthesis,
    semicolon,
  )
import Lib.AST.Type (pType)
import Lib.AST.Util (pVisibilitySpecifier)
import Lib.Parser
  ( Parser,
    pIdentifier,
    pMany1Stop,
    pManySpaces,
    pOneKeyword,
  )
import Text.Parsec

pState :: Parser Stat
pState =
  StatAssign <$> try pAssignStat
    <|> StatVarDef <$> try pStVarDefStatement
    <|> StatIf <$> try pIfStatement
    <|> StatFor <$> try pForStatement
    <|> StatWhile <$> try pWhileStatement
    <|> StatDoWhile <$> try pDoWhileStatement
    <|> try pContinue
    <|> try pBreak
    <|> try pReturn

pAssignStat :: Parser StAssignStatement
pAssignStat =
  liftA2
    (\var expr -> StAssignStatement {stAssignVarName = var, stAssignExpr = expr})
    (pIdentifier <* pManySpaces <* pOneKeyword "=" <* pManySpaces)
    pExpression
    <* pOneKeyword semicolon

pStVarDefStatement :: Parser StVarDefStatement
pStVarDefStatement = do
  tp <- pType
  mmemory <- optionMaybe pLocationModifer
  name <- pManySpaces >> pIdentifier
  expr <-
    optionMaybe $
      pManySpaces
        >> pOneKeyword "="
        >> pManySpaces
        >> pExpression
  _ <- pOneKeyword semicolon

  c <- pManySpaces >> optionMaybe pComment
  return
    StVarDefStatement
      { stVarType = tp,
        stVarName = name,
        stVarExpr = expr,
        stVarLocation = fromMaybe Storage mmemory,
        stVarComment = c
      }

-- state varaible is stored in the chain storage, so it rejects the memory keyword
pStateVariable :: Parser StateVariable
pStateVariable = do
  tp <- pType
  visual <- optionMaybe $ try pVisibilitySpecifier
  name <- pManySpaces >> pIdentifier
  expr <-
    optionMaybe $
      pManySpaces
        *> pOneKeyword "="
        *> pManySpaces
        *> pExpression
  _ <- pOneKeyword semicolon
  c <- pManySpaces *> optionMaybe pComment
  return
    StateVariable
      { svType = tp,
        svName = name,
        svVarExpr = expr,
        svVisibleSpecifier = fromMaybe VsInternal visual,
        svComment = c
      }

pIfStatement :: Parser IfStatement
pIfStatement =
  try pStateSingleIfStat
    <|> pStateComplexIfElse

pStateElse :: Parser [Stat]
pStateElse = do
  -- if no else clause, the parser fails directly
  mayebElseIf <-
    pManySpaces
      *> pOneKeyword "else"
      *> pManySpaces
      *> optionMaybe
        ( -- don't consume the if because the further parser requires the 'if' keyword
          lookAhead (pOneKeyword "if")
        )
  case mayebElseIf of
    -- no sub if-else, only have one else for the if-else clause
    Nothing ->
      between
        (pManySpaces *> pOneKeyword leftCurlyBrace <* pManySpaces)
        (pManySpaces *> pOneKeyword rightCurlyBrace <* pManySpaces)
        (many $ pState <* pManySpaces)
    -- we treat the else-if and else as a seperated if-else clause
    Just _ -> fmap (: []) $ StatIf <$> pIfStatement

pStateComplexIfElse :: Parser IfStatement
pStateComplexIfElse = do
  _ <-
    pManySpaces
      *> pOneKeyword "if"
      *> pManySpaces
  cond <-
    between
      (pOneKeyword leftParenthesis)
      (pOneKeyword rightParenthesis)
      (pManySpaces *> pExpression <* pManySpaces)

  ifClauses <-
    pManySpaces
      *> between
        (pOneKeyword leftCurlyBrace)
        (pOneKeyword rightCurlyBrace)
        (many $ pManySpaces *> pState <* pManySpaces)
  mayebElse <- optionMaybe pStateElse
  return
    IfStatement
      { stIfCond = cond,
        stIfThen = ifClauses,
        stIfElse = fromMaybe [] mayebElse
      }

-- we parse the one line if without else and curly brackets
-- if (amount > msg.value / 2 ether)
--     revert("Not enough Ether provided.");
pStateSingleIfStat :: Parser IfStatement
pStateSingleIfStat = do
  _ <-
    pManySpaces
      *> pOneKeyword "if"
      *> pManySpaces
  cond <-
    between
      (pOneKeyword leftParenthesis)
      (pOneKeyword rightParenthesis)
      (pManySpaces *> pExpression <* pManySpaces)
  stat <- pManySpaces *> pState
  return
    IfStatement
      { stIfCond = cond,
        stIfThen = [stat],
        stIfElse = []
      }

pContinue :: Parser Stat
pContinue = pManySpaces >> pOneKeyword "continue" >> return StatContinue

pBreak :: Parser Stat
pBreak = pManySpaces >> pOneKeyword "break" >> return StatBreak

pReturn :: Parser Stat
pReturn =
  pManySpaces
    >> pOneKeyword "return"
    >> pManySpaces
    >> StatReturn
      <$> optionMaybe pExpression
      <* pManySpaces
      <* pOneKeyword semicolon

pForStatement :: Parser ForStatement
pForStatement = do
  _ <-
    pManySpaces
      *> pOneKeyword "for"
      *> pManySpaces

  (stat, expr, cond) <-
    between
      (pOneKeyword leftParenthesis)
      (pOneKeyword rightParenthesis)
      ( do
          stat <- pManySpaces >> optionMaybe pStVarDefStatement
          case stat of
            Nothing -> pManySpaces *> pOneKeyword semicolon >> pure ()
            Just _ -> pure ()
          expr <- pManySpaces >> optionMaybe pExpression <* pManySpaces <* pOneKeyword semicolon
          cond <- pManySpaces >> optionMaybe pExpression <* pManySpaces
          return (stat, expr, cond)
      )
  body <-
    pManySpaces
      >> between
        (pManySpaces *> pOneKeyword leftCurlyBrace <* pManySpaces)
        (pManySpaces *> pOneKeyword rightCurlyBrace <* pManySpaces)
        (many $ pState <* pManySpaces)
  return
    ForStatement
      { forDecl = stat,
        forExprStat = expr,
        forCond = cond,
        forBody = body
      }

pWhileStatement :: Parser WhileStatement
pWhileStatement = do
  _ <-
    pManySpaces
      *> pOneKeyword "while"
      *> pManySpaces
  cond <-
    between
      (pOneKeyword leftParenthesis)
      (pOneKeyword rightParenthesis)
      (pManySpaces >> pExpression <* pManySpaces)
  body <-
    pManySpaces
      >> between
        (pManySpaces *> pOneKeyword leftCurlyBrace <* pManySpaces)
        (pManySpaces *> pOneKeyword rightCurlyBrace <* pManySpaces)
        (many $ pState <* pManySpaces)
  return
    WhileStatement
      { whileCond = cond,
        whileBody = body
      }

pDoWhileStatement :: Parser DoWhileStatement
pDoWhileStatement = do
  _ <-
    pManySpaces
      *> pOneKeyword "do"
      *> pManySpaces
  body <-
    pManySpaces
      >> between
        (pManySpaces *> pOneKeyword leftCurlyBrace <* pManySpaces)
        (pManySpaces *> pOneKeyword rightCurlyBrace <* pManySpaces)
        (many $ pState <* pManySpaces)

  cond <-
    pManySpaces
      *> pOneKeyword "while"
      *> pManySpaces
      *> between
        (pOneKeyword leftParenthesis)
        (pOneKeyword rightParenthesis)
        (pManySpaces >> pExpression <* pManySpaces)
      <* pOneKeyword semicolon

  return
    DoWhileStatement
      { doWhileCond = cond,
        doWhileBody = body
      }
