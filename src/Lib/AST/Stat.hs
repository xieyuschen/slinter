module Lib.AST.Stat where

import Control.Applicative (Applicative (..), optional)
import Data.Maybe (fromMaybe)
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
