module Lib.AST.Contract where

import Control.Applicative (Alternative (many, (<|>)))
import Data.Maybe (mapMaybe)
import Lib.AST.Function
  ( getCtFunction,
    getCtVariable,
    pFunction,
  )
import Lib.AST.Model
  ( Contract (..),
    ContractField (CtComment, CtEmptyLine, CtFunction, CtVariable),
    keywordContract,
    leftCurlyBrace,
    rightCurlyBrace,
  )
import Lib.AST.Pragma (pComment)
import Lib.AST.Stat (pStateVariable)
import Lib.Parser
  ( Parser,
    pIdentifier,
    pManySpaces,
    pOneKeyword,
  )
import Text.Parsec (manyTill, try)
import Text.ParserCombinators.Parsec (newline)

-- contract Counter {
--     uint256 public count;

--     // Function to get the current count
--     function get() public view returns (uint256) {
--         return count;
--     }
-- }
pContract :: Parser Contract
pContract = do
  contractName <-
    pManySpaces
      >> pOneKeyword keywordContract
      >> pManySpaces
        *> pIdentifier
        <* pManySpaces
        <* pManySpaces

  fields <-
    pManySpaces
      >> pOneKeyword leftCurlyBrace
      >> manyTill
        ( fmap CtFunction (try pFunction)
            <|> fmap CtVariable (try pStateVariable)
            <|> fmap CtComment (try pComment)
            <|> (pManySpaces >> many newline >> return CtEmptyLine)
        )
        (pOneKeyword rightCurlyBrace)

  let fns = mapMaybe getCtFunction fields
  let vars = mapMaybe getCtVariable fields
  return
    Contract
      { ctName = contractName,
        ctFunctions = fns,
        ctVariables = vars
      }
