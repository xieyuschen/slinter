module Lib.AST.Stat where

import Control.Applicative (Alternative ((<|>)), Applicative (..), optional)
import Data.Maybe (fromMaybe, isJust)
import Lib.AST.Comment (pComment)
import Lib.AST.Expr
import Lib.AST.Model
import Lib.AST.Type (pType)
import Lib.AST.Util (pVisibilitySpecifier)
import Lib.Parser

pAssignStat :: Parser StAssign
pAssignStat =
  liftA2
    (\var expr -> StAssign {stAssignVarName = var, stAssignExpr = expr})
    (pManySpaces >> pIdentifier <* (pManySpaces >> pOneKeyword "="))
    pExpression
    <* pOneKeyword semicolon

pStVarDefinition :: Parser StVarDefinition
pStVarDefinition = do
  tp <- pType
  mmemory <- optional pLocationModifer
  name <- pManySpaces >> pIdentifier
  expr <-
    optional $
      pManySpaces
        >> pOneKeyword "="
        >> pManySpaces
        >> pExpression
  _ <- pOneKeyword semicolon
  c <- pManySpaces >> optional pComment
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
  visual <- optional $ pTry pVisibilitySpecifier
  name <- pManySpaces >> pIdentifier
  expr <-
    optional $
      pManySpaces
        >> pOneKeyword "="
        >> pManySpaces
        >> pExpression
  _ <- pOneKeyword semicolon
  c <- pManySpaces >> optional pComment
  return
    StateVariable
      { svType = tp,
        svName = name,
        svVarExpr = expr,
        svVisibleSpecifier = fromMaybe VsInternal visual,
        svComment = c
      }
