module Lib.AST.Comment where

import Data.Char (isSpace)
import Lib.AST.Model
  ( Comment,
    Pragma,
    keywordPragma,
    keywordSolidity,
  )
import Lib.Parser
  ( Parser,
    pManySpaces,
    pOneKeyword,
    pReadline,
    pSemVer,
  )

-- // SPDX-License-Identifier: MIT
pSPDXComment :: Parser String
pSPDXComment = do
  _ <- pOneKeyword "// SPDX-License-Identifier:" >> pManySpaces
  content <- pReadline
  let (license, _) = break isSpace content
  return license

-- // helloworld
pComment :: Parser Comment
pComment = do
  _ <- pOneKeyword "//"
  pReadline

-- pragma solidity ^0.8.24;
pPragma :: Parser Pragma
pPragma = do
  _ <-
    pManySpaces
      >> pOneKeyword keywordPragma
      >> pManySpaces
      >> pOneKeyword keywordSolidity
      >> pManySpaces
  version <- pSemVer
  _ <- pOneKeyword ";"
  return version
