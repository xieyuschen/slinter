module Lib.AST.CommentSpec (spec) where

import Control.Monad (forM_)
import Lib.AST.Comment
import Lib.Parser
import Lib.TestCommon
import Test.Hspec

spec :: Spec
spec = do
  parseCommentSpec
  parseSPDXCommentSpec
  parsePragmaSpec

parseCommentSpec :: Spec
parseCommentSpec = do
  let testCases =
        [ ( "//helloworld _*&^",
            Right "helloworld _*&^",
            ""
          ),
          ( "//helloworld _*&^\n",
            Right "helloworld _*&^",
            ""
          ),
          ( "//   helloworld _*&^\n",
            Right "   helloworld _*&^",
            ""
          )
        ]
  forM_ testCases $ verifyParser "comment" pComment

parseSPDXCommentSpec :: Spec
parseSPDXCommentSpec = do
  let testCases =
        [ ( "// SPDX-License-Identifier: MIT",
            Right "MIT",
            ""
          ),
          ( "// SPDX-License-Identifier: BSD-2",
            Right "BSD-2",
            ""
          )
        ]
  forM_ testCases $ verifyParser "spdx comment" pSPDXComment

parsePragmaSpec :: Spec
parsePragmaSpec = do
  let testCases =
        [ ( "pragma solidity ^0.8.24;",
            Right (SemVer {major = 0, minor = 8, patch = Just 24, semVerRangeMark = Just Caret}),
            ""
          ),
          ( "pragma solidity ~0.8.24;",
            Right (SemVer {major = 0, minor = 8, patch = Just 24, semVerRangeMark = Just Tilde}),
            ""
          ),
          ( "pragma solidity *;",
            Right (SemVer {major = 0, minor = 0, patch = Nothing, semVerRangeMark = Just Wildcards}),
            ""
          ),
          ( "pragma solidity 0.8.24;",
            Right (SemVer {major = 0, minor = 8, patch = Just 24, semVerRangeMark = Nothing}),
            ""
          )
        ]
  forM_ testCases $ verifyParser "pragma" pPragma
