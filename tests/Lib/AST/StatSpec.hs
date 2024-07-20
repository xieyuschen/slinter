{-# LANGUAGE OverloadedStrings #-}

module Lib.AST.StatSpec (spec) where

import Control.Monad (forM_)
import Lib.AST.Model
  ( DataLocation (Memory, Storage),
    ExprBinary (ExprBinary, bOperator, leftOperand, rightOperand),
    ExprFnCall (ExprFnCall, fnArguments, fnContractName, fnName),
    ExprIndex (ExprIndex, elemBase, elemIndex),
    ExprSelection (ExprSelection, selectionBase, selectionField),
    FnCallArgs (FnCallArgsList),
    Literal (LNum),
    Operator (ArithmeticAddition, Minus),
    SExpr (SExprB, SExprF, SExprI, SExprL, SExprS, SExprVar),
    SType (STypeFixed, STypeUint),
    StAssign (StAssign, stAssignExpr, stAssignVarName),
    StVarDefinition
      ( StVarDefinition,
        stVarComment,
        stVarExpr,
        stVarLocation,
        stVarName,
        stVarType
      ),
    StateVariable
      ( StateVariable,
        svComment,
        svName,
        svType,
        svVarExpr,
        svVisibleSpecifier
      ),
    VisibilitySpecifier (VsInternal, VsPublic),
  )
import Lib.AST.Stat
  ( pAssignStat,
    pStVarDefinition,
    pStateVariable,
  )
import Lib.TestCommon (verifyParser)
import Test.Hspec (Spec)

spec :: Spec
spec = do
  parseStatAssignSpec
  parseVarDefinitionSpec
  parseStateVarSpec

parseVarDefinitionSpec :: Spec
parseVarDefinitionSpec = do
  let testCases =
        [ ( "uint256 public name = hello;",
            Left "fail to find desired charactor: ';';",
            "name = hello;"
          ),
          ( "fixed hello=2345;",
            Right
              StVarDefinition
                { stVarType = STypeFixed 128 18,
                  stVarName = "hello",
                  stVarLocation = Storage,
                  stVarExpr = Just (SExprL (LNum 2345)),
                  stVarComment = Nothing
                },
            ""
          ),
          ( "fixed memory hello=2345;",
            Right
              StVarDefinition
                { stVarType = STypeFixed 128 18,
                  stVarName = "hello",
                  stVarLocation = Memory,
                  stVarExpr = Just (SExprL (LNum 2345)),
                  stVarComment = Nothing
                },
            ""
          )
        ]
  forM_ testCases $ verifyParser "variable definition" pStVarDefinition

parseStateVarSpec :: Spec
parseStateVarSpec = do
  let testCases =
        [ ( "uint256 public name;",
            Right
              StateVariable
                { svVisibleSpecifier = VsPublic,
                  svType = STypeUint 256,
                  svName = "name",
                  svComment = Nothing,
                  svVarExpr = Nothing
                },
            ""
          ),
          ( "uint256 public name; // 123daws",
            Right
              StateVariable
                { svVisibleSpecifier = VsPublic,
                  svType = STypeUint 256,
                  svName = "name",
                  svComment = Just " 123daws",
                  svVarExpr = Nothing
                },
            ""
          ),
          ( "uint256 public name = hello;",
            Right
              StateVariable
                { svVisibleSpecifier = VsPublic,
                  svType = STypeUint 256,
                  svName = "name",
                  svComment = Nothing,
                  svVarExpr = Just (SExprVar "hello")
                },
            ""
          ),
          ( "fixed hello=2345;",
            Right
              StateVariable
                { svVisibleSpecifier = VsInternal,
                  svType = STypeFixed 128 18,
                  svName = "hello",
                  svComment = Nothing,
                  svVarExpr = Just (SExprL (LNum 2345))
                },
            ""
          ),
          ( "fixed memory hello=2345;",
            Left "fail to find desired charactor: ';';", -- state variable rejects the memory modifier
            "hello=2345;"
          )
        ]
  forM_ testCases $ verifyParser "state variable definition" pStateVariable

parseStatAssignSpec :: Spec
parseStatAssignSpec = do
  let testCases =
        [ ( "owner = msg.sender;",
            Right $
              StAssign
                { stAssignVarName = "owner",
                  stAssignExpr =
                    SExprS $
                      ExprSelection
                        { selectionBase = SExprVar "msg",
                          selectionField = "sender"
                        }
                },
            ""
          ),
          ( "owner = 1+2-3;",
            Right
              StAssign
                { stAssignVarName = "owner",
                  stAssignExpr =
                    SExprB
                      ExprBinary
                        { leftOperand =
                            SExprB
                              ExprBinary
                                { leftOperand = SExprL (LNum 1),
                                  rightOperand = SExprL (LNum 2),
                                  bOperator = ArithmeticAddition
                                },
                          rightOperand = SExprL (LNum 3),
                          bOperator = Minus
                        }
                },
            ""
          ),
          ( "owner = msg[x].sender;",
            Right
              StAssign
                { stAssignVarName = "owner",
                  stAssignExpr =
                    SExprS
                      ExprSelection
                        { selectionBase =
                            SExprI
                              ExprIndex
                                { elemBase = SExprVar "msg",
                                  elemIndex = SExprVar "x"
                                },
                          selectionField = "sender"
                        }
                },
            ""
          ),
          ( "owner = ctname.sender();",
            Right
              StAssign
                { stAssignVarName = "owner",
                  stAssignExpr =
                    SExprF
                      ExprFnCall
                        { fnContractName = Just "ctname",
                          fnName = "sender",
                          fnArguments = FnCallArgsList []
                        }
                },
            ""
          ),
          ( "owner = ctname.sender()",
            Left "fail to find desired charactor: ';';",
            ""
          )
        ]

  forM_ testCases $ verifyParser "assign statement" pAssignStat
