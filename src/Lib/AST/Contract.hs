module Lib.AST.Contract where

import Control.Applicative
import Control.Applicative (Alternative ((<|>)), optional)
import Data.Maybe (mapMaybe)
import Lib.AST.Comment (pComment)
import Lib.AST.Function
  ( getCtFunction,
    getCtVariable,
    pFunction,
  )
import Lib.AST.Model
  ( Contract (..),
    ContractField (CtComment, CtFunction, CtVariable),
    StateVariable (..),
    keywordContract,
    leftCurlyBrace,
    rightCurlyBrace,
  )
import Lib.AST.Stat (pStateVariable)
import Lib.AST.Type (pType)
import Lib.AST.Util (pVisibilitySpecifier)
import Lib.Parser
  ( Parser,
    pIdentifier,
    pManySpaces,
    pOneKeyword,
  )

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
        <* ( pManySpaces
               >> pOneKeyword leftCurlyBrace
               >> pManySpaces
           )
  fields <-
    many
      ( fmap CtFunction pFunction
          <|> fmap CtVariable pStateVariable
          <|> fmap CtComment pComment
      )
      <* pOneKeyword rightCurlyBrace

  let fns = mapMaybe getCtFunction fields
  let vars = mapMaybe getCtVariable fields
  return
    Contract
      { ctName = contractName,
        ctFunctions = fns,
        ctVariables = vars
      }
