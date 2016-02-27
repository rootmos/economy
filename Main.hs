module Main where

import Month
import Economy

import System.FilePath
import System.Directory
import Options.Applicative
import qualified Data.ByteString.Lazy as B

data Config = Config { dataFilename :: FilePath
                     , subcommand :: SubCommand}

data SubCommand = ShowMonth ShowMonthOptions | ShowYear

data ShowMonthOptions = ShowMonthOptions { showMonthCmdMonth :: Month }


configParser :: FilePath -> Parser Config
configParser defaultFilePath = Config <$> strOption ( long "file"
                                    <> short 'f'
                                    <> metavar "FILE"
                                    <> value defaultFilePath
                                    <> help ("the file to load data from (defaults to " ++ defaultFilePath ++ ")"))
                      <*> subcommandParser

subcommandParser :: Parser SubCommand
subcommandParser = subparser ( command "month" (info (helper <*> showMonthParser) (progDesc "Show details for a month"))
                             <> command "year" (info (helper <*> showYearParser) (progDesc "Show the whole year"))
                             )


showMonthParser :: Parser SubCommand
showMonthParser = ShowMonth <$> showMonthOptionsParser

showMonthOptionsParser :: Parser ShowMonthOptions
showMonthOptionsParser = ShowMonthOptions <$> argument auto ( metavar "MONTH" )

showYearParser :: Parser SubCommand
showYearParser = pure ShowYear

fromFile :: FilePath -> IO Economy
fromFile path = do
    datafileContent <- B.readFile path 
    case eitherDecode datafileContent of
      Left err -> fail err
      Right economy -> return economy 


run :: Economy -> SubCommand -> IO ()
run economy (ShowMonth options) = detailsOfMonth economy (showMonthCmdMonth options) 
run economy ShowYear = detailsOfYear economy

getDefaultFilePath :: IO FilePath
getDefaultFilePath = do
    home <- getHomeDirectory
    return $ home </> ".config" </> "economy.json"

main :: IO ()
main = do
    defaultFilePath <- getDefaultFilePath
    config <- execParser $ info (helper <*> configParser defaultFilePath) fullDesc
    economy <- fromFile (dataFilename config)
    run economy (subcommand config)

