{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Maybe (fromJust, isJust, isNothing)
import Data.Text (Text)
import Lib.Command (executeFile, executeProject)
import Options.Applicative
import System.Exit (exitFailure)

data Flags = Flags
  { filePath :: Maybe Text,
    projectPath :: Maybe Text
  }

type ErrMsg = Text

validateFlags :: Flags -> Maybe ErrMsg
validateFlags Flags {..}
  | isNothing filePath && isNothing projectPath =
      Just "Error: Either --file or --project must be specified."
  | isJust filePath && isJust projectPath =
      Just "Error: Either --file or --project must be specified."
  | otherwise = Nothing

flagsParser :: Parser Flags
flagsParser =
  Flags
    <$> optional
      ( strOption
          ( long "file"
              <> short 'f'
              <> metavar "TARGET"
              <> help "the relative/absolute path of the file to lint"
          )
      )
    <*> optional
      ( strOption
          ( long "project"
              <> short 'p'
              <> metavar "TARGET"
              <> help "the relative/absolute path of the folder to lint all files with sol extension"
          )
      )

opts :: ParserInfo Flags
opts =
  info
    (flagsParser <**> helper)
    ( fullDesc
        <> progDesc "please specify the TARGET via flag for slinter to lint"
        <> header "slinter: a solidity linter to ensure a better quality"
    )

main :: IO ()
main = do
  flags <- execParser opts
  let eFlags = validateFlags flags
  _ <- maybe (return ()) (\err -> print err >> exitFailure) eFlags
  execute flags

execute :: Flags -> IO ()
execute Flags {..}
  | isJust filePath = executeFile (fromJust filePath)
  | isJust projectPath = executeProject (fromJust projectPath)
  | otherwise = print ("unreachable" :: Text)
