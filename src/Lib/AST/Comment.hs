module Lib.AST.Comment where

import Data.Char (isSpace)
import Lib.AST.Model
import Lib.Parser

-- // SPDX-License-Identifier: MIT
pSPDXComment :: Parser String
pSPDXComment = do
  content <- pOneKeyword "// SPDX-License-Identifier:" >> pManySpaces >> pReadline
  let (license, _) = break isSpace content
  return license

-- // helloworld
pComment :: Parser Comment
pComment = do
  pOneKeyword "//" >> pReadline

-- pragma solidity ^0.8.24;
pPragma :: Parser Pragma
pPragma = do
  pManySpaces
    >> pOneKeyword keywordPragma
    >> pManySpaces
    >> pOneKeyword keywordSolidity
    >> pManySpaces
    >> pSemVer
      <* pOneKeyword ";"
