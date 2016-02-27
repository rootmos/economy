module Main where

import Economy
import Options.Applicative
import qualified Data.ByteString.Lazy as B

data Config = Config { dataFilename :: FilePath }


defaultFilePath :: String
defaultFilePath = "~/.config/economy.json"

configParser :: Parser Config
configParser = Config <$> strOption ( long "file"
                                    <> short 'f'
                                    <> metavar "FILE"
                                    <> value defaultFilePath
                                    <> help ("the file to load data from (defaults to " ++ defaultFilePath ++ ")"))

fromFile :: FilePath -> IO Economy
fromFile path = do
    datafileContent <- B.readFile path 
    case eitherDecode datafileContent of
      Left err -> fail err
      Right economy -> return economy 

main :: IO ()
main = do
    config <- execParser $ info (helper <*> configParser) fullDesc
    economy <- fromFile (dataFilename config)
    summarize economy

