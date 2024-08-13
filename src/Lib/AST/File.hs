module Lib.AST.File where

import Lib.AST.Definition
import Lib.AST.Function
import Lib.AST.Model (SolFile (..), SolFileSum (..))
import Lib.AST.Pragma
import Lib.AST.Stat (pStateVariable)
import Lib.AST.Type (pTypeEnum, pTypeStruct, pUserDefinedValueTypeDefinition)
import Lib.Parser
import Text.Parsec (sepEndBy, try, (<|>))

pWholeSolFile :: Parser SolFile
pWholeSolFile = do
  spdx <- pManySpaces *> pSPDXComment
  pragma <- pManySpaces *> pPragma

  all <-
    pManySpaces
      *> sepEndBy
        ( (SolFileSumImportDirective <$> try pImportDirective)
            <|> (SolFileSumUsingDirective <$> try pUsingDirective)
            <|> (SolFileSumContractDefinition <$> try pContractDefinition)
            <|> (SolFileSumInterfaceDefinition <$> try pInterfaceDefinition)
            <|> (SolFileSumLibraryDefinition <$> try pLibraryDefinition)
            <|> (SolFileSumFunctionDefinition <$> try pFunctionDefinition)
            <|> (SolFileSumStateVariable <$> try pStateVariable)
            <|> (SolFileSumStructure <$> try pTypeStruct)
            <|> (SolFileSumSTypeEnum <$> try pTypeEnum)
            <|> (SolFileSumUserDefinedValueTypeDefinition <$> try pUserDefinedValueTypeDefinition)
            <|> (SolFileSumErrorDefinition <$> try pErrorDefinition)
            <|> (SolFileSumEventDefinition <$> try pEventDefinition)
        )
        pManySpaces

  return $
    -- todo: finish me with the real data
    SolFile
      { solUsingDirectives = [],
        solUserDefineValueType = [],
        solStructs = [],
        solSpdxLicense = spdx,
        solPragma = pragma,
        solLibraries = [],
        solInterfaces = [],
        solImprotDirectives = [],
        solFunctions = [],
        solFileName = [],
        solEvents = [],
        solErrors = [],
        solEnums = [],
        solContracts = [],
        solConstantVars = []
      }
