module Main where

import Lib.Lexier
    ( parseMany,
      parseOne,
      parseSpace,
      Parser,
      Token(Operator, Identifier, Keyword),
      parseThen,
      parseIdentifier )
import Control.Monad.State
import Control.Monad.Trans.Except (runExceptT)

data AST = Struct {
                    name :: String
                  }
          | TODO deriving (Show)

structCom :: Parser AST
structCom = do
    _ <- parseOne "type" Keyword 
    _ <- parseMany parseSpace
    result <- parseIdentifier
    let ident = case result of
                  Identifier i -> i
                  _ -> error "expected identifier" 
    _ <- parseMany parseSpace
    _ <- parseOne "struct" Keyword
    _ <- parseMany parseSpace
    _ <- parseOne "{" Operator
    _ <- parseMany parseSpace
    _ <- parseOne "}" Operator
    return (Struct {name = ident })

main :: IO ()
main = do
  let input = "type Helloworld struct {  }"
  print $ evalState (runExceptT structCom) input
