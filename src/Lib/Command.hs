{-# LANGUAGE OverloadedStrings #-}

module Lib.Command (executeFile, executeProject) where

import Data.Text (Text, pack, unpack)
import Lib.AST.File (pWholeSolFile)
import Lib.AST.Parser (runSParser)
import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist)
import System.Directory.Internal.Prelude (exitFailure)
import Text.Printf (printf)

data ProvidedPath = File | Folder deriving (Show)

ensureExist :: Text -> ProvidedPath -> (FilePath -> IO Bool) -> IO (Either Text FilePath)
ensureExist path p exist = do
  abs <- canonicalizePath $ unpack path
  fileExists <- exist abs
  if fileExists
    then return $ Right abs
    else return $ Left (pack $ printf "%s %s doesn't exist" (show p) abs)

executeFile :: Text -> IO ()
executeFile filepath = do
  ei <- ensureExist filepath File doesFileExist
  ensuredPath <- either (\err -> print err >> exitFailure) return ei
  fileContent <- pack <$> readFile ensuredPath
  let (re, _) = runSParser (pWholeSolFile ensuredPath) fileContent
  case re of
    Right file -> print file
    Left err -> print err >> exitFailure
  return ()

executeProject :: Text -> IO ()
executeProject folderPath = do
  ei <- ensureExist folderPath Folder doesDirectoryExist
  ensuredPath <- either (\err -> print err >> exitFailure) return ei
  print "unsupported faeture for a project"
