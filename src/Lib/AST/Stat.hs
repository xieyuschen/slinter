{-# LANGUAGE OverloadedStrings #-}

module Lib.AST.Stat where

import Control.Applicative (liftA2)
import Data.Maybe (fromMaybe)
import Lib.AST.Comment (pComment)
import Lib.AST.Expr (pExpression, pLocationModifer)
import Lib.AST.Model
  ( DataLocation (Storage),
    StAssign (..),
    StVarDefinition (..),
    StateVariable (..),
    VisibilitySpecifier (VsInternal),
    semicolon,
  )
import Lib.AST.Type (pType)
import Lib.AST.Util (pVisibilitySpecifier)
import Lib.Parser
  ( Parser,
    pIdentifier,
    pManySpaces,
    pOneKeyword,
  )
import Text.Parsec

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
