{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Arithmetics
import Data.Aeson
import Data.Scientific
import Data.Maybe
import Data.List (transpose)
import Options.Applicative
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import GHC.Generics
import Text.PrettyPrint.Boxes

newtype Money = Money Int
    deriving (Eq, Ord)

instance Show Money where
    show (Money x) = show x

instance Num Money where
    (Money n) + (Money m) = Money (n + m)
    (Money n) * (Money m) = Money (n * m)
    negate (Money n) = Money (negate n)
    abs (Money n) = Money (abs n)
    fromInteger n = Money (fromInteger n)
    signum (Money n) = Money (signum n)

type Name = String

data Economy = Economy { incomes :: [Income], expenses :: [Expense] }
    deriving (Show, Generic)
instance FromJSON Economy

instance FromJSON Money where
    parseJSON (Number n) = pure $ Money (fromJust $ toBoundedInteger n)
    parseJSON (String s) = case arithmetics (T.unpack s) of
                             Left e -> fail e
                             Right n -> pure (Money n)
    parseJSON _ = empty


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

instance ToMoney Economy where
    money economy = sum (map money $ incomes economy) + sum (map money $ expenses economy)

class MaybeMontly a where
    willOccurInMonth :: a -> Int -> Bool

instance MaybeMontly Expense where
    willOccurInMonth (Expense { expenseMonths = Just months }) month = month `elem` months
    willOccurInMonth _ _ = True

instance MaybeMontly Income where
    willOccurInMonth (Income { incomeMonths = Just months }) month = month `elem` months
    willOccurInMonth _ _ = True

data Config = Config { dataFilename :: FilePath }

configParser :: Parser Config
configParser = Config <$> argument str (metavar "DATA")

montlyEconomy :: Economy -> Int -> Economy
montlyEconomy economy month = economy { incomes = filter (`willOccurInMonth` month) (incomes economy)
                                      , expenses = filter (`willOccurInMonth` month) (expenses economy)
                                      }

summarize :: Economy -> IO ()
summarize economy = printBox $ hsep 2 left [names, amounts]
    where
        names = vcat left . map text $ columnified !! 0
        amounts = vcat right . map text $ columnified !! 1
        columnified = transpose $ listify economy

listify :: Economy -> [[String]]
listify economy = (map listifyIncome (incomes economy)) ++ (map listifyExpense (expenses economy)) ++ separator ++ summarow
    where
        listifyIncome i = map ($ i) [incomeName, show . money, maybeEmptyString . incomeMonths]
        listifyExpense e = map ($ e) [expenseName, show . money, maybeEmptyString . expenseMonths]
        separator = [["---", "---", "---"]]
        summarow = [["", show . money $ economy, ""]]

maybeEmptyString :: Show a => Maybe a -> String
maybeEmptyString (Just x) = show x
maybeEmptyString Nothing = ""

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
    summarize $ montlyEconomy economy 5

