{-# LANGUAGE OverloadedStrings #-}

module Lib.AST.Definition where

import Data.Functor (($>))
import Data.Maybe (listToMaybe)
import Lib.AST.Model (EventDefinition (..), EventParameter (..), FnDecorator (..), ModifierDefinition (..), extractFnDecOs, leftCurlyBrace, leftParenthesis, rightCurlyBrace, rightParenthesis, semicolon)
import Lib.AST.Stat (pState)
import Lib.AST.Type (pInt, pType)
import Lib.AST.Util
import Lib.Parser (Parser, pIdentifier, pMany1Spaces, pManySpaces, pNumber, pOneKeyword)
import Text.Parsec (between, char, many, optionMaybe, sepBy, (<|>))

pModifierDefinition :: Parser ModifierDefinition
pModifierDefinition = do
  ident <-
    pOneKeyword "modifier"
      *> pMany1Spaces
      *> pIdentifier
  args <- pManySpaces >> optionMaybe pFunctionArgsInParentheses

  dec <-
    sepBy
      ( pFnDeclVirtual
          <|> (FnDecOs <$> pOverrideSpecifier)
      )
      pMany1Spaces

  modifierBody <-
    pManySpaces
      *> pOneKeyword semicolon
      $> Nothing
      <|> Just
        <$> between
          (pOneKeyword leftCurlyBrace >> pManySpaces)
          (pOneKeyword rightCurlyBrace)
          (many $ pState <* pManySpaces)
  return
    ModifierDefinition
      { modifierName = ident,
        modifierParamList = args,
        modifierIsVirtual = FnDecVirtual `elem` dec,
        modifierOverrideSpecifier = listToMaybe $ extractFnDecOs dec,
        modifierBody = modifierBody
      }

-- data EventParameter = EventParameter{
--   eventParamName :: Text,
--   eventParamIndex :: Maybe Int,
--   eventParamIdent :: Maybe Text
-- }deriving (Show, Eq)
-- data EventDefinition = EventDefinition{
--   eventName :: Text,
--   eventParameter :: [EventParameter],
--   eventIsAnonymous :: Bool
-- } deriving (Show, Eq)

pEventParameter :: Parser EventParameter
pEventParameter = do
  tp <- pType
  index <- optionMaybe pNumber <* pManySpaces
  ident <- optionMaybe pIdentifier
  return
    EventParameter
      { eventParamIdent = ident,
        eventParamIndex = index,
        eventParamType = tp
      }

pEventDefinition :: Parser EventDefinition
pEventDefinition = do
  ident <-
    pOneKeyword "event"
      *> pMany1Spaces
      *> pIdentifier
  params <-
    between
      (pOneKeyword leftParenthesis *> pManySpaces)
      (pOneKeyword rightParenthesis *> pManySpaces)
      ( sepBy (pManySpaces *> pEventParameter <* pManySpaces) (char ',')
      )

  isAnonymous <- pOneKeyword "anonymous" $> True <|> return False
  _ <- pOneKeyword semicolon
  return
    EventDefinition
      { eventParameters = params,
        eventIsAnonymous = isAnonymous,
        eventName = ident
      }
