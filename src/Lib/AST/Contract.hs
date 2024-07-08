module Lib.AST.Contract where

import Control.Applicative (Alternative ((<|>)))
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
    pMany,
    pManySpaces,
    pOne,
    pOneKeyword,
    pOpt,
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
  _ <-
    pManySpaces
      >> pOneKeyword keywordContract
      >> pManySpaces
  contractName <- pIdentifier
  _ <-
    pManySpaces
      >> pOneKeyword leftCurlyBrace
      >> pManySpaces
  fields <-
    pMany
      ( fmap CtFunction pFunction
          <|> fmap CtVariable pStateVariable
          <|> fmap CtComment pComment
      )

  _ <- pOne rightCurlyBrace id
  let fns = mapMaybe getCtFunction fields
  let vars = mapMaybe getCtVariable fields
  return
    Contract
      { cname = contractName,
        cfunctions = fns,
        cvariables = vars
      }

-- 'uint256 public count;' under the contract scope
-- todo: shall we call it pContractStateVariable? check it when we implement to parse the function body
pStateVariable :: Parser StateVariable
pStateVariable = do
  tp <- pManySpaces >> pType
  -- state variable only has visiblity specifier, we don't need to parse the modifiers
  specifier <- pManySpaces >> pVisibilitySpecifier
  stateName <- pManySpaces >> pIdentifier
  _ <-
    pManySpaces
      >> pOne ";" id
  comment <- pManySpaces >> pOpt pComment
  return
    ( StateVariable
        { svVisibleSpecifier = specifier,
          svType = tp,
          svName = stateName,
          svComment = comment
        }
    )
