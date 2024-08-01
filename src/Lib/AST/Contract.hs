{-# LANGUAGE OverloadedStrings #-}

module Lib.AST.Contract where

import Control.Applicative (Alternative (many, (<|>)))
import Control.Monad (when)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Lib.AST.Definition (pContractBody, pInheritanceSpecifier)
import Lib.AST.Model
  ( ContractDefinition (..),
    keywordContract,
    leftCurlyBrace,
    rightCurlyBrace,
  )
import Lib.AST.Pragma (pComment)
import Lib.AST.Stat (pStateVariable)
import Lib.Parser
  ( Parser,
    pIdentifier,
    pMany1Spaces,
    pManySpaces,
    pOneKeyword,
  )
import Text.Parsec

pContractDefinition :: Parser ContractDefinition
pContractDefinition = do
  -- if the abstract keyword exist, we must consume at least 1 space
  abs <-
    pManySpaces
      *> optionMaybe (try $ pOneKeyword "abstract")
  when (isJust abs) pMany1Spaces
  contractName <-
    pOneKeyword keywordContract
      >> pMany1Spaces
        *> pIdentifier
        <* pManySpaces
  inheriSpeciciers <-
    optionMaybe $
      pOneKeyword "is"
        *> pMany1Spaces
        *> sepBy
          (pManySpaces *> pInheritanceSpecifier <* pManySpaces)
          (char ',')

  body <-
    pManySpaces
      *> between
        (pManySpaces >> pOneKeyword leftCurlyBrace >> pManySpaces)
        (pManySpaces >> pOneKeyword rightCurlyBrace >> pManySpaces)
        pContractBody
  return
    ContractDefinition
      { contractName = contractName,
        contractIsAbstract = isJust abs,
        contractInheritanceSpecifiers = fromMaybe [] inheriSpeciciers,
        contractBody = body
      }
