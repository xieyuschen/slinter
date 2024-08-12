{-# LANGUAGE OverloadedStrings #-}

module Lib.AST.Definition where

import Data.Either
import Data.Functor (($>))
import Data.Maybe (fromMaybe, listToMaybe)
import Lib.AST.Expr (pFnCallArgs)
import Lib.AST.Function (pFnDeclModifierInvocation, pFunctionDefinition)
import Lib.AST.Model (ConstructorDefinition (..), ConstructorMutability (..), ContractBody (..), ErrorDefinition (..), ErrorParameter (ErrorParameter, errParamName, errParamType), EventDefinition (..), EventParameter (..), FnDecorator (..), FnModifierInvocation, FnName (..), FunctionDefinition (fnDefName), InheritanceSpecifier (..), InterfaceDefinition (..), LibraryDefinition (..), ModifierDefinition (..), extractFnDecOs, leftCurlyBrace, leftParenthesis, rightCurlyBrace, rightParenthesis, semicolon, CBFSSum (..))
import Lib.AST.Pragma (pComment, pUsingDirective)
import Lib.AST.Stat (pState, pStateVariable)
import Lib.AST.Type (pInt, pType, pTypeEnum, pTypeStruct, pUserDefinedValueTypeDefinition)
import Lib.AST.Util
import Lib.Parser (Parser, pIdentifier, pMany1Spaces, pManySpaces, pNumber, pOneKeyword)
import Text.Parsec (between, char, many, optionMaybe, sepBy, sepEndBy, try, (<|>))

pConstructorMutability :: Parser ConstructorMutability
pConstructorMutability =
  do
    try (pOneKeyword "payable") $> ConstructorPayable
    <|> try (pOneKeyword "internal") $> ConstructorInternal
    <|> try (pOneKeyword "public") $> ConstructorPublic

pModifierDefinition :: Parser ModifierDefinition
pModifierDefinition = do
  ident <-
    pOneKeyword "modifier"
      *> ( pMany1Spaces
             <|> fail "space is required after keyword 'modifier'"
         )
      *> pIdentifier
  args <- pManySpaces >> optionMaybe pFnDeclArgsInParentheses

  dec <-
    sepBy
      ( pFnDeclVirtual
          <|> (FnDecOs <$> pOverrideSpecifier)
      )
      pMany1Spaces

  modifierBody <-
    pManySpaces
      *> pOneKeyword semicolon
      $> Nothing
      <|> Just
        <$> between
          (pOneKeyword leftCurlyBrace >> pManySpaces)
          (pOneKeyword rightCurlyBrace)
          (many $ pState <* pManySpaces)
  return
    ModifierDefinition
      { modifierName = ident,
        modifierParamList = args,
        modifierIsVirtual = FnDecVirtual `elem` dec,
        modifierOverrideSpecifier = listToMaybe $ extractFnDecOs dec,
        modifierBody = modifierBody
      }

pEventParameter :: Parser EventParameter
pEventParameter = do
  tp <- pType
  index <- optionMaybe pNumber <* pManySpaces
  ident <- optionMaybe pIdentifier
  return
    EventParameter
      { eventParamIdent = ident,
        eventParamIndex = index,
        eventParamType = tp
      }

pEventDefinition :: Parser EventDefinition
pEventDefinition = do
  ident <-
    pOneKeyword "event"
      *> ( pMany1Spaces
             <|> fail "space is required after keyword 'event'"
         )
      *> pIdentifier
  params <-
    between
      (pOneKeyword leftParenthesis *> pManySpaces)
      (pOneKeyword rightParenthesis *> pManySpaces)
      ( sepBy (pManySpaces *> pEventParameter <* pManySpaces) (char ',')
      )

  isAnonymous <- pOneKeyword "anonymous" $> True <|> return False
  _ <- pManySpaces *> pOneKeyword semicolon
  return
    EventDefinition
      { eventParameters = params,
        eventIsAnonymous = isAnonymous,
        eventName = ident
      }

pErrorDefinition :: Parser ErrorDefinition
pErrorDefinition = do
  ident <-
    pOneKeyword "error"
      *> ( pMany1Spaces
             <|> fail "space is required after keyword 'error'"
         )
      *> pIdentifier
      <* pManySpaces
  args <-
    between
      (pOneKeyword leftParenthesis)
      (pOneKeyword rightParenthesis)
      ( sepBy (pManySpaces *> pErrorParameter <* pManySpaces) (char ',')
      )
  _ <- pManySpaces *> pOneKeyword semicolon
  return $
    ErrorDefinition
      { errParameters = args,
        errName = ident
      }

pErrorParameter :: Parser ErrorParameter
pErrorParameter = do
  tp <- pType <* pManySpaces
  nameM <- optionMaybe pIdentifier
  return $
    ErrorParameter
      { errParamName = nameM,
        errParamType = tp
      }

pInheritanceSpecifier :: Parser InheritanceSpecifier
pInheritanceSpecifier = do
  path <- pIdentifierPath
  args <- optionMaybe pFnCallArgs
  return $
    InheritanceSpecifier
      { inheritanceCallArgs = args,
        inheritancePath = path
      }

pConstructorDefinition :: Parser ConstructorDefinition
pConstructorDefinition = do
  parameters <-
    pOneKeyword "constructor"
      *> pFnDeclArgsInParentheses

  decorators <-
    sepBy
      ( Left <$> pConstructorMutability
          <|> Right <$> pFnDeclModifierInvocation
      )
      pMany1Spaces

  body <-
    pManySpaces
      *> between
        (pOneKeyword leftCurlyBrace >> pManySpaces)
        (pOneKeyword rightCurlyBrace)
        (many $ pState <* pManySpaces)
  let mutability = fromMaybe ConstructorPublic $ listToMaybe $ lefts decorators
      mi = listToMaybe $ rights decorators

  return $
    ConstructorDefinition
      { constructorBody = body,
        constructorMutability = mutability,
        constructorModifierInvocation = mi,
        constructorParamList = parameters
      }

-- pContractBody parses the contract body quoted with '{}'
pContractBody :: Parser ContractBody
pContractBody = do
  all <-
    sepEndBy
      (   CBFSSumComment <$> try pComment 
          <|> CBFSSumUsingDirective <$> try pUsingDirective
          <|> CBFSSumConstructorDefinition <$> try pConstructorDefinition
          <|> CBFSSumFunctionDefinition <$> try pFunctionDefinition
          <|> CBFSSumModifierDefinition <$> try pModifierDefinition
          <|> CBFSSumStructure <$> try pTypeStruct
          <|> CBFSSumSTypeEnum <$> try pTypeEnum
          <|> CBFSSumUserDefinedValueTypeDefinition <$> try pUserDefinedValueTypeDefinition
          <|> CBFSSumEventDefinition <$> try pEventDefinition
          <|> CBFSSumErrorDefinition <$> try pErrorDefinition
          -- the state variable parser should be put at last to avoid parse the other keyword as a type
          <|> CBFSSumStateVariable <$> try pStateVariable
      )
      pManySpaces

  return $
    ContractBody
      { ctBodyConstructor = Nothing,
        ctBodyFunctions = [v | CBFSSumFunctionDefinition v <- all],
        ctBodyModifiers = [v | CBFSSumModifierDefinition v <- all],
        ctBodyStructDefinitions = [v | CBFSSumStructure v <- all],
        ctBodyEnumDefinitions = [v | CBFSSumSTypeEnum v <- all],
        ctBodyUserDefinedValueTypeDefinition = [v | CBFSSumUserDefinedValueTypeDefinition v <- all],
        ctBodyStateVariables = [v | CBFSSumStateVariable v <- all],
        ctBodyEventDefinitions = [v | CBFSSumEventDefinition v <- all],
        ctBodyErrorDefinitions = [v | CBFSSumErrorDefinition v <- all],
        ctBodyUsingDirectives = [v | CBFSSumUsingDirective v <- all],
        ctBodyReceiveFunctions = filter (\f -> fnDefName f == FnReceive) [v | CBFSSumFunctionDefinition v <- all],
        ctBodyFallbackFunctions = filter (\f -> fnDefName f == FnFallback) [v | CBFSSumFunctionDefinition v <- all]
        -- ctBodyAllFields = all
      }

pInterfaceDefinition :: Parser InterfaceDefinition
pInterfaceDefinition = do
  name <-
    pManySpaces
      *> pOneKeyword "interface"
      *> ( pMany1Spaces
             <|> fail "space is required after keyword 'interface'"
         )
      *> pIdentifier
      <* pManySpaces

  iSpecicier <-
    optionMaybe $
      try
        ( pOneKeyword "is"
            *> pMany1Spaces
            *> sepBy
              (pManySpaces *> pInheritanceSpecifier <* pManySpaces)
              (char ',')
        )

  body <-
    pManySpaces
      *> between
        (pOneKeyword leftCurlyBrace)
        (pOneKeyword rightCurlyBrace)
        (pManySpaces *> pContractBody <* pManySpaces)

  return $
    InterfaceDefinition
      { interfaceName = name,
        interfaceInheritanceSpecifiers = fromMaybe [] iSpecicier,
        interfaceBody = body
      }

pLibraryDefinition :: Parser LibraryDefinition
pLibraryDefinition = do
  name <-
    pManySpaces
      *> pOneKeyword "library"
      *> (pMany1Spaces <|> fail "space is required after keyword 'library'")
      *> pIdentifier
      <* pManySpaces
  body <-
    pManySpaces
      *> between
        (pOneKeyword leftCurlyBrace)
        (pOneKeyword rightCurlyBrace)
        (pManySpaces *> pContractBody <* pManySpaces)
  return $
    LibraryDefinition
      { libraryBody = body,
        libraryName = name
      }
