module Lib.AST.Contract where

import Control.Applicative (Alternative (empty), (<|>))
import Control.Arrow (Arrow (first))
import Control.Monad.Except (ExceptT (ExceptT), MonadError (throwError), guard)
import Control.Monad.State (MonadState (..))
import Control.Monad.Trans.Except (ExceptT (ExceptT))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import Data.Bits (Bits (bit))
import Data.Char (isAlpha, isAlphaNum, isDigit, isNumber, isSpace)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, mapMaybe)
import Data.Text.Lazy.Builder.Int (decimal)
import Debug.Trace (trace)
import GHC.Base (Applicative (..), Type)
import GHC.Generics (URec (UInt))
import Lib.AST.Comment
import Lib.AST.Function
import Lib.AST.Model
import Lib.AST.Type
import Lib.Parser
import Text.Read (Lexeme (String), readMaybe)

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
  name <- pIdentifier
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
      { cname = name,
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
  name <- pManySpaces >> pIdentifier
  _ <-
    pManySpaces
      >> pOne ";" id
  comment <- pManySpaces >> pOpt pComment
  return
    ( StateVariable
        { svVisibleSpecifier = specifier,
          svType = tp,
          svName = name,
          svComment = comment
        }
    )
