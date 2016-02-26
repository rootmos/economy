{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Data.Aeson
import Options.Applicative
import qualified Data.ByteString.Lazy as B
import GHC.Generics

type Money = Int
type Name = String

data Economy = Economy { incomes :: [Income] , expenses :: [Expense] }
    deriving (Show, Generic)
instance FromJSON Economy


data Expense = Expense { expenseName :: Name
                       , expenseAmount :: Money
                       , expenseMonths :: Maybe [Int]
                       }
    deriving Show

instance FromJSON Expense where
    parseJSON (Object v) = Expense <$> 
        v .: "name" <*>
        v .: "amount" <*>
        v .:? "months"
    parseJSON _ = empty


data Income = Income { incomeName :: Name
                     , incomeAmount :: Money
                     , incomeMonths :: Maybe [Int]
                     }
    deriving Show

instance FromJSON Income where
    parseJSON (Object v) = Income <$> 
        v .: "name" <*>
        v .: "amount" <*>
        v .:? "months"
    parseJSON _ = empty


class ToMoney a where
    money :: a -> Money

instance ToMoney Expense where
    money = negate . expenseAmount

instance ToMoney Income where
    money = incomeAmount

class ToMoney a => MontlyTransaction a where
    willOccurInMonth :: a -> Int -> Bool

    amount :: a -> Int -> Maybe Money
    amount x m = if x `willOccurInMonth` m then (Just (money x)) else Nothing

instance MontlyTransaction Expense where
    willOccurInMonth (Expense { expenseMonths = Just months }) month = month `elem` months
    willOccurInMonth _ _ = True

instance MontlyTransaction Income where
    willOccurInMonth (Income { incomeMonths = Just months }) month = month `elem` months
    willOccurInMonth _ _ = True

data Config = Config { dataFilename :: FilePath }

configParser :: Parser Config
configParser = Config <$> argument str (metavar "DATA")

montlyEconomy :: Economy -> Int -> Economy
montlyEconomy economy month = economy { incomes = filter (`willOccurInMonth` month) (incomes economy)
                                      , expenses = filter (`willOccurInMonth` month) (expenses economy)
                                      }
balance :: Economy -> Money
balance economy = sum (map money $ incomes economy) + sum (map money $ expenses economy)

main :: IO ()
main = do
    config <- execParser $ info (helper <*> configParser) fullDesc
    datafileContent <- B.readFile (dataFilename config)
    let eitherEconomy = eitherDecode datafileContent :: Either String Economy
    case eitherEconomy of
      Left err -> fail err
      Right economy -> putStrLn . show . balance $ montlyEconomy economy 5

