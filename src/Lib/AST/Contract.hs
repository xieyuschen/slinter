module Lib.AST.Contract where

import Control.Applicative
import Control.Applicative (Alternative ((<|>)), optional)
import Data.Maybe (mapMaybe)
import Lib.AST.Comment (pComment)
import Lib.AST.Function
  ( getCtFunction,
    getCtVariable,
    pFunction,
    pVisibilitySpecifier,
  )
import Lib.AST.Model
  ( Contract (..),
    ContractField (CtComment, CtFunction, CtVariable),
    StateVariable (..),
    keywordContract,
    leftCurlyBrace,
    rightCurlyBrace,
  )
import Lib.AST.Type (pType)
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

-- 'uint256 public count;' under the contract scope
-- todo: shall we call it pContractStateVariable? check it when we implement to parse the function body
pStateVariable :: Parser StateVariable
pStateVariable = do
  tp <- pManySpaces >> pType
  -- state variable only has visiblity specifier, we don't need to parse the modifiers
  specifier <- pManySpaces >> pVisibilitySpecifier
  stateName <-
    pManySpaces
      >> pIdentifier
        <* ( pManySpaces
               >> pOneKeyword ";"
           )

  comment <- pManySpaces >> optional pComment
  return
    ( StateVariable
        { svVisibleSpecifier = specifier,
          svType = tp,
          svName = stateName,
          svComment = comment
        }
    )
