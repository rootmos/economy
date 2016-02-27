{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Economy ( Economy, eitherDecode, summarize ) where

import Arithmetics
import Month
import Data.Aeson
import Data.Aeson.Types
import Data.Scientific
import Data.Maybe
import Data.List (transpose)
import qualified Data.Text as T
import GHC.Generics
import Text.PrettyPrint.Boxes hiding ((<>))

-- Types

type Name = String


-- Classes

class ToMoney a where
    money :: a -> Money

class MaybeMontly a where
    willOccurInMonth :: a -> Month -> Bool


-- Money type

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

instance FromJSON Money where
    parseJSON (Number n) = pure $ Money (fromJust $ toBoundedInteger n)
    parseJSON (String s) = case arithmetics (T.unpack s) of
                             Left e -> fail e
                             Right n -> pure (Money n)
    parseJSON invalid = typeMismatch "Money" invalid



-- Economy type

data Economy = Economy { incomes :: [Income], expenses :: [Expense] }
    deriving (Show, Generic)
instance FromJSON Economy

instance ToMoney Economy where
    money economy = sum (map money $ incomes economy) + sum (map money $ expenses economy)


-- Expense type

data Expense = Expense { expenseName :: Name
                       , expenseAmount :: Money
                       , expenseMonths :: Maybe [Month]
                       }
    deriving Show

instance FromJSON Expense where
    parseJSON (Object v) = Expense <$> 
        v .: "name" <*>
        v .: "amount" <*>
        v .:? "months"
    parseJSON invalid = typeMismatch "Expense" invalid

instance ToMoney Expense where
    money = negate . expenseAmount

instance MaybeMontly Expense where
    willOccurInMonth (Expense { expenseMonths = Just months }) month = month `elem` months
    willOccurInMonth _ _ = True


-- Income type

data Income = Income { incomeName :: Name
                     , incomeAmount :: Money
                     , incomeMonths :: Maybe [Month]
                     }
    deriving Show

instance FromJSON Income where
    parseJSON (Object v) = Income <$> 
        v .: "name" <*>
        v .: "amount" <*>
        v .:? "months"
    parseJSON invalid = typeMismatch "Income" invalid

instance ToMoney Income where
    money = incomeAmount

instance MaybeMontly Income where
    willOccurInMonth (Income { incomeMonths = Just months }) month = month `elem` months
    willOccurInMonth _ _ = True


monthlyEconomy :: Economy -> Month -> Economy
monthlyEconomy economy month = economy { incomes = filter (`willOccurInMonth` month) (incomes economy)
                                      , expenses = filter (`willOccurInMonth` month) (expenses economy)
                                      }

summarize :: Economy -> IO ()
summarize = printBox .economyBox

economyBox :: Economy -> Box
economyBox economy = hsep 1 left [names, amounts]
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

