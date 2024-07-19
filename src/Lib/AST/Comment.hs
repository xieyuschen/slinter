module Lib.AST.Comment where

import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T
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
pSPDXComment :: Parser Text
pSPDXComment = do
  content <- pOneKeyword "// SPDX-License-Identifier:" >> pManySpaces >> pReadline
  let (license, _) = T.break isSpace content
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
