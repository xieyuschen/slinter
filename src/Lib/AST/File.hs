module Lib.AST.File where

import Data.Text (Text)
import Lib.AST.Definition
import Lib.AST.Function
import Lib.AST.Model (SolFile (..), SolFileSum (..), StateVariable (svConstrains), StateVariableConstrain (SVarConstant))
import Lib.AST.Parser
import Lib.AST.Pragma
import Lib.AST.Stat (pStateVariable)
import Lib.AST.Type (pTypeEnum, pTypeStruct, pUserDefinedValueTypeDefinition)
import Text.Parsec (sepEndBy, try, (<|>))

pWholeSolFile :: FilePath -> Parser SolFile
pWholeSolFile filename = do
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
    SolFile
      { solUsingDirectives = [v | SolFileSumUsingDirective v <- all],
        solUserDefineValueType = [v | SolFileSumUserDefinedValueTypeDefinition v <- all],
        solStructs = [v | SolFileSumStructure v <- all],
        solSpdxLicense = spdx,
        solPragma = pragma,
        solLibraries = [v | SolFileSumLibraryDefinition v <- all],
        solInterfaces = [v | SolFileSumInterfaceDefinition v <- all],
        solImprotDirectives = [v | SolFileSumImportDirective v <- all],
        solFunctions = [v | SolFileSumFunctionDefinition v <- all],
        solFileName = filename,
        solEvents = [v | SolFileSumEventDefinition v <- all],
        solErrors = [v | SolFileSumErrorDefinition v <- all],
        solEnums = [v | SolFileSumSTypeEnum v <- all],
        solContracts = [v | SolFileSumContractDefinition v <- all],
        solStateVars = [v | SolFileSumStateVariable v <- all],
        solConstantVars = [v | SolFileSumStateVariable v <- all, SVarConstant `elem` svConstrains v]
      }
