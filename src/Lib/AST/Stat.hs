{-# LANGUAGE OverloadedStrings #-}

module Lib.AST.Stat where

import Control.Applicative (liftA2)
import Control.Monad.Trans.Accum (look)
import Data.Maybe (fromMaybe)
import Lib.AST.Comment (pComment)
import Lib.AST.Expr (pExpression, pLocationModifer)
import Lib.AST.Model
  ( DataLocation (Storage),
    StAssign (..),
    StVarDefinition (..),
    Stat (..),
    StateVariable (..),
    StstIfElse (..),
    VisibilitySpecifier (VsInternal),
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
  do
    StatAssign <$> try pAssignStat
    <|> StatVarDef <$> try pStVarDefinition
    <|> StatIfElse <$> try pStateIfElse

pAssignStat :: Parser StAssign
pAssignStat =
  liftA2
    (\var expr -> StAssign {stAssignVarName = var, stAssignExpr = expr})
    (pIdentifier <* pManySpaces <* pOneKeyword "=" <* pManySpaces)
    pExpression
    <* pOneKeyword semicolon

pStVarDefinition :: Parser StVarDefinition
pStVarDefinition = do
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
    StVarDefinition
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

pStateIfElse :: Parser StstIfElse
pStateIfElse =
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
    Just _ -> fmap (: []) $ StatIfElse <$> pStateIfElse

pStateComplexIfElse :: Parser StstIfElse
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
    StstIfElse
      { stIfCond = cond,
        stIfThen = ifClauses,
        stIfElse = fromMaybe [] mayebElse
      }

-- we parse the one line if without else and curly brackets
-- if (amount > msg.value / 2 ether)
--     revert("Not enough Ether provided.");
pStateSingleIfStat :: Parser StstIfElse
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
    StstIfElse
      { stIfCond = cond,
        stIfThen = [stat],
        stIfElse = []
      }
