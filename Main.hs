module Main where

import Economy
import Options.Applicative
import qualified Data.ByteString.Lazy as B
import Month

data Config = Config { dataFilename :: FilePath
                     , subcommand :: SubCommand}

data SubCommand = ShowMonth ShowMonthOptions

data ShowMonthOptions = ShowMonthOptions { showMonthCmdMonth :: Month }

defaultFilePath :: String
defaultFilePath = "~/.config/economy.json"

configParser :: Parser Config
configParser = Config <$> strOption ( long "file"
                                    <> short 'f'
                                    <> metavar "FILE"
                                    <> value defaultFilePath
                                    <> help ("the file to load data from (defaults to " ++ defaultFilePath ++ ")"))
                      <*> subcommandParser

subcommandParser :: Parser SubCommand
subcommandParser = subparser (command "month" (info (helper <*> showMonthParser) (progDesc "Show details for a month")))

showMonthParser :: Parser SubCommand
showMonthParser = ShowMonth <$> showMonthOptionsParser

showMonthOptionsParser :: Parser ShowMonthOptions
showMonthOptionsParser = ShowMonthOptions <$> argument auto ( metavar "MONTH" )

fromFile :: FilePath -> IO Economy
fromFile path = do
    datafileContent <- B.readFile path 
    case eitherDecode datafileContent of
      Left err -> fail err
      Right economy -> return economy 


run :: Economy -> SubCommand -> IO ()
run economy (ShowMonth options) = summarize $ monthlyEconomy economy (showMonthCmdMonth options) 

main :: IO ()
main = do
    config <- execParser $ info (helper <*> configParser) fullDesc
    economy <- fromFile (dataFilename config)
    run economy (subcommand config)

