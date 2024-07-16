module Lib.AST.Stat where

import Control.Applicative (Applicative (..))
import Lib.AST.Expr
import Lib.AST.Model
import Lib.Parser

pAssignStat :: Parser StAssign
pAssignStat =
  liftA2
    (\var expr -> StAssign {stAssignVarName = var, stAssignExpr = expr})
    (pManySpaces >> pIdentifier <* (pManySpaces >> pOneKeyword "="))
    pExpression
    <* pOneKeyword semicolon
