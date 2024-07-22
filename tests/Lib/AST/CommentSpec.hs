{-# LANGUAGE OverloadedStrings #-}

module Lib.AST.CommentSpec (spec) where

import Control.Monad (forM_)
import Lib.AST.Comment (pComment, pPragma, pSPDXComment)
import Lib.Parser
  ( SemVer (SemVer, major, minor, patch, semVerRangeMark),
    SemVerRangeMark (Caret, Tilde, Wildcards),
  )
import Lib.TestCommon
import Test.Hspec (Spec)

spec :: Spec
spec = do
  parseCommentSpec
  parseSPDXCommentSpec
  parsePragmaSpec

parseCommentSpec :: Spec
parseCommentSpec = do
  let testCases =
        [ ( "//helloworld _*&^\r\n",
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
          ),
          ( "// helloworld _*&^",
            Left "", -- no ending line, won't be treated as a comment
            "// helloworld _*&^"
          )
        ]
  forM_ testCases $ verifyParser "comment" pComment

parseSPDXCommentSpec :: Spec
parseSPDXCommentSpec = do
  let testCases =
        [ ( "// SPDX-License-Identifier: MIT\n",
            Right "MIT",
            ""
          ),
          ( "// SPDX-License-Identifier: BSD-2\n",
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
