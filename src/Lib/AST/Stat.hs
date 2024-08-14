{-# LANGUAGE OverloadedStrings #-}

module Lib.AST.Stat
  ( pState,
    pStateVariable,
    pEmitStatement,
    pCatchStatment,
    pTryStatement,
    pDoWhileStatement,
    pWhileStatement,
    pForStatement,
    pIfStatement,
    pAssignStat,
    pStVarDefStatement,
    pRevertStatement,
  )
where

import Control.Applicative (liftA2)
import Control.Exception (catches)
import Control.Monad (when)
import Control.Monad.Cont (guard)
import Control.Monad.Trans.Accum (look)
import Data.Maybe (fromMaybe, isNothing, listToMaybe)
import Data.Text (Text)
import Debug.Trace (trace)
import Lib.AST.Expr (pExpression, pFuncCallArgsFormatList)
import Lib.AST.Model
  ( CatchStatement (..),
    DataLocation (Storage),
    DoWhileStatement (DoWhileStatement, doWhileBody, doWhileCond),
    EmitStatement (EmitStatement, emitCallArgs, emitEventIdent),
    FnCallArgs,
    FnVisibility (FnInternal),
    ForStatement (..),
    IfStatement (..),
    RevertStatement (..),
    SExpr,
    StAssignStatement (..),
    StVarDefStatement (..),
    Stat (..),
    StateVariable (..),
    StateVariableConstrain (..),
    TryStatement (..),
    WhileStatement (..),
    leftCurlyBrace,
    leftParenthesis,
    rightCurlyBrace,
    rightParenthesis,
    semicolon,
  )
import Lib.AST.Pragma (pComment)
import Lib.AST.Type (pType)
import Lib.AST.Util
  ( pFunctionArgs,
    pLocationModifier,
    pStateVariableConstrain,
  )
import Lib.Parser
  ( Parser,
    pIdentifier,
    pMany1Spaces,
    pManySpaces,
    pOneKeyword,
  )
import Text.Parsec
  ( between,
    getInput,
    lookAhead,
    many,
    optionMaybe,
    sepBy,
    try,
    (<|>),
  )

pState :: Parser Stat
pState =
  try pContinue
    <|> try pBreak
    <|> try pReturn
    <|> StatAssign <$> try pAssignStat
    <|> StatVarDef <$> try pStVarDefStatement
    <|> StatIf <$> try pIfStatement
    <|> StatFor <$> try pForStatement
    <|> StatWhile <$> try pWhileStatement
    <|> StatDoWhile <$> try pDoWhileStatement
    <|> StatTry <$> try pTryStatement
    <|> StatEmit <$> try pEmitStatement
    <|> StatRevert <$> try pRevertStatement
    -- put the expr statement at the last to avoid some keywords are treated as expression
    <|> StatExpr <$> try pExprStatment
    <|> StatComment <$> try pComment

pExprStatment :: Parser SExpr
pExprStatment = pManySpaces *> pExpression <* pManySpaces <* pOneKeyword semicolon

pAssignStat :: Parser StAssignStatement
pAssignStat =
  liftA2
    (\var expr -> StAssignStatement {stAssignVarName = var, stAssignExpr = expr})
    (pIdentifier <* pManySpaces <* pOneKeyword "=" <* pManySpaces)
    pExpression
    <* pManySpaces
    <* pOneKeyword semicolon

pStVarDefStatement :: Parser StVarDefStatement
pStVarDefStatement = do
  tp <- pType
  mmemory <- optionMaybe pLocationModifier
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
pStateVariable =
  try pConstStateVariable
    <|> pStateVariableOptionalInit

-- constant state variable must have an expression to evaluate for initialization
pConstStateVariable :: Parser StateVariable
pConstStateVariable = do
  tp <- pType
  cs <- many $ pStateVariableConstrain <* pMany1Spaces
  guard (SVarConstant `elem` map (const SVarConstant) cs)
  name <- pManySpaces >> pIdentifier
  expr <-
    pManySpaces
      *> pOneKeyword "="
      *> pManySpaces
      *> pExpression
  c <-
    pOneKeyword semicolon
      *> pManySpaces
      *> optionMaybe pComment
  return
    StateVariable
      { svType = tp,
        svName = name,
        svVarExpr = Just expr,
        svConstrains = cs,
        svComment = c
      }

pStateVariableOptionalInit :: Parser StateVariable
pStateVariableOptionalInit = do
  tp <- pType -- pType will try to consume all chars for identifier, needn't pMany1Spaces
  cs <- many $ pStateVariableConstrain <* pMany1Spaces
  -- when we found constant here, we can make sure the pConstStateVariable fails, so error report is necessary
  when (SVarConstant `elem` cs) (fail "constant must be initialized")

  let constrains = case cs of
        [] -> [SVarInternal]
        _ -> cs

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
        svConstrains = constrains,
        svComment = c
      }

pIfStatement :: Parser IfStatement
pIfStatement =
  try pStateSingleIfStat
    <|> (pManySpaces >> pStateComplexIfElse)

pStateElse :: Parser [Stat]
pStateElse = do
  -- if no else clause, the parser fails directly
  mayebElseIf <-
    pManySpaces
      *> pOneKeyword "else"
      -- we need to handle the cases: "else if", "else{"
      -- but the case of "elseif" is invalid
      *> optionMaybe
        ( -- don't consume the if because the further parser requires the 'if' keyword
          -- it should have at least one space between else and if
          -- use try to avoid the pMany1Spaces partially take effect
          lookAhead $ try (pMany1Spaces *> pOneKeyword "if")
        )
  case mayebElseIf of
    -- no sub if-else, only have one else for the if-else clause
    Nothing ->
      between
        (pManySpaces *> pOneKeyword leftCurlyBrace <* pManySpaces)
        (pManySpaces *> pOneKeyword rightCurlyBrace <* pManySpaces)
        (many $ pState <* pManySpaces)
    -- we treat the else-if and else as a seperated if-else clause
    -- at least
    Just _ -> fmap (: []) $ StatIf <$> (pMany1Spaces >> pIfStatement)

-- the caller should consume the leading spaces,
-- sometimes the caller needs to ensure there must at least one leading space to avoid 'elseif' case
pStateComplexIfElse :: Parser IfStatement
pStateComplexIfElse = do
  _ <-
    pOneKeyword "if"
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
    >> StatReturn
      <$> optionMaybe (try $ pMany1Spaces *> pExpression)
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

pTryStatement :: Parser TryStatement
pTryStatement = do
  expr <-
    between
      (pManySpaces >> pOneKeyword "try" >> pMany1Spaces)
      (pMany1Spaces >> pOneKeyword "returns" >> pManySpaces)
      pExpression
  args <-
    between
      (pManySpaces *> pOneKeyword leftParenthesis <* pManySpaces)
      (pManySpaces *> pOneKeyword rightParenthesis <* pManySpaces)
      pFunctionArgs

  body <-
    between
      (pManySpaces >> pOneKeyword leftCurlyBrace >> pManySpaces)
      (pManySpaces >> pOneKeyword rightCurlyBrace >> pManySpaces)
      (many pState)
  catches <- many pCatchStatment
  return
    TryStatement
      { tryExpr = expr,
        tryReturns = args,
        tryBody = body,
        tryCatches = catches
      }

pCatchStatment :: Parser CatchStatement
pCatchStatment = do
  ident <-
    pOneKeyword "catch"
      >> pMany1Spaces
      >> optionMaybe pIdentifier

  -- consume more spaces only when the identifier is not exist
  when (isNothing ident) pManySpaces

  args <-
    between
      -- no leading space is allowed
      -- because the identifier and parenthesis should be attached together
      (pOneKeyword leftParenthesis <* pManySpaces)
      (pManySpaces *> pOneKeyword rightParenthesis <* pManySpaces)
      pFunctionArgs

  body <-
    between
      (pManySpaces >> pOneKeyword leftCurlyBrace >> pManySpaces)
      (pManySpaces >> pOneKeyword rightCurlyBrace >> pManySpaces)
      (many $ pState <* pManySpaces)
  return
    CatchStatement
      { catchIdent = ident,
        catchParams = args,
        catchBody = body
      }

-- defined for parsing 'emit' and 'revert' statement
pEventCall :: Text -> Parser (Text, FnCallArgs)
pEventCall keyword = do
  ident <-
    pManySpaces
      >> pOneKeyword keyword
      >> pMany1Spaces
      >> pIdentifier
  args <- pFuncCallArgsFormatList
  return (ident, args)

pEmitStatement :: Parser EmitStatement
pEmitStatement = do
  (ident, args) <- pEventCall "emit"
  return
    EmitStatement
      { emitEventIdent = ident,
        emitCallArgs = args
      }

pRevertStatement :: Parser RevertStatement
pRevertStatement = do
  (ident, args) <- pEventCall "revert"
  return
    RevertStatement
      { revertEventIdent = ident,
        revertCallArgs = args
      }
