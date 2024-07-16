module Lib.AST.StatSpec (spec) where

import Control.Monad
import Lib.AST.Model
import Lib.AST.Stat
import Lib.TestCommon
import Test.Hspec

spec :: Spec
spec = parseStatAssignSpec

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