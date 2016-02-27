{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Month (Month) where

import Test.Hspec
import Data.List
import Data.Char
import Data.Maybe
import Data.Aeson
import Data.Aeson.Types
import Data.Scientific
import qualified Data.Text as T

newtype Month = Month Int
    deriving (Eq, Num)

instance Show Month where
    show (Month i) = months !! (i-1)

instance Read Month where
    readsPrec _ = reader

instance FromJSON Month where
    parseJSON (Number n) | n >= 1 && n <= 12 = pure $ Month (fromJust $ toBoundedInteger n)
    parseJSON (String s) = case parseMonth (T.unpack s) of
                             Nothing -> fail ("Unable to parse " ++ T.unpack s)
                             Just m -> pure m
    parseJSON invalid = typeMismatch "Month" invalid


months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
lowerCaseMonths = map (map toLower) months

parseMonth :: String -> Maybe Month
parseMonth string = case findIndices (isPrefixOf (map toLower string)) lowerCaseMonths of
                     [x] -> Just (Month (x+1))
                     _ -> Nothing

reader :: ReadS Month
reader string = catMaybes $ map ($ string) (map (monthTester []) months)

monthTester :: String -> String -> String -> Maybe (Month, String)
monthTester matched [] i = case parseMonth (reverse matched) of
                             Just m -> Just (m, i)
                             Nothing -> Nothing
monthTester matched _ [] = case parseMonth (reverse matched) of
                             Just m -> Just (m, [])
                             Nothing -> Nothing
monthTester matched (x:xs) i@(y:ys)
    | toLower x == toLower y = monthTester (x:matched) xs ys 
    | otherwise = case parseMonth (reverse matched) of
                    Just m -> Just (m, i)
                    Nothing -> Nothing

main :: IO ()
main = hspec $ do
    describe "parseMonth" $ do
        it "parses January to 1" $ do parseMonth "January" `shouldBe` Just (Month 1)
        it "parses February to 2" $ do parseMonth "February" `shouldBe` Just (Month 2)
        it "parses March to 3" $ do parseMonth "March" `shouldBe` Just (Month 3)
        it "parses April to 4" $ do parseMonth "April" `shouldBe` Just (Month 4)
        it "parses May to 5" $ do parseMonth "May" `shouldBe` Just (Month 5)
        it "parses June to 6" $ do parseMonth "June" `shouldBe` Just (Month 6)
        it "parses July to 7" $ do parseMonth "July" `shouldBe` Just (Month 7)
        it "parses August to 8" $ do parseMonth "August" `shouldBe` Just (Month 8)
        it "parses September to 9" $ do parseMonth "September" `shouldBe` Just (Month 9)
        it "parses October to 10" $ do parseMonth "October" `shouldBe` Just (Month 10)
        it "parses November to 11" $ do parseMonth "November" `shouldBe` Just (Month 11)
        it "parses December to 12" $ do parseMonth "December" `shouldBe` Just (Month 12)

        it "parses january to 1" $ do parseMonth "january" `shouldBe` Just (Month 1)
        it "parses february to 2" $ do parseMonth "february" `shouldBe` Just (Month 2)
        it "parses march to 3" $ do parseMonth "march" `shouldBe` Just (Month 3)
        it "parses april to 4" $ do parseMonth "april" `shouldBe` Just (Month 4)
        it "parses may to 5" $ do parseMonth "may" `shouldBe` Just (Month 5)
        it "parses june to 6" $ do parseMonth "june" `shouldBe` Just (Month 6)
        it "parses july to 7" $ do parseMonth "july" `shouldBe` Just (Month 7)
        it "parses august to 8" $ do parseMonth "august" `shouldBe` Just (Month 8)
        it "parses september to 9" $ do parseMonth "september" `shouldBe` Just (Month 9)
        it "parses october to 10" $ do parseMonth "october" `shouldBe` Just (Month 10)
        it "parses november to 11" $ do parseMonth "november" `shouldBe` Just (Month 11)
        it "parses december to 12" $ do parseMonth "december" `shouldBe` Just (Month 12)

        it "parses ja to 1" $ do parseMonth "ja" `shouldBe` Just (Month 1)
        it "parses f to 2" $ do parseMonth "f" `shouldBe` Just (Month 2)
        it "parses mar to 3" $ do parseMonth "mar" `shouldBe` Just (Month 3)
        it "parses ap to 4" $ do parseMonth "ap" `shouldBe` Just (Month 4)
        it "parses may to 5" $ do parseMonth "may" `shouldBe` Just (Month 5)
        it "parses jun to 6" $ do parseMonth "jun" `shouldBe` Just (Month 6)
        it "parses jul to 7" $ do parseMonth "jul" `shouldBe` Just (Month 7)
        it "parses au to 8" $ do parseMonth "au" `shouldBe` Just (Month 8)
        it "parses s to 9" $ do parseMonth "s" `shouldBe` Just (Month 9)
        it "parses o to 10" $ do parseMonth "o" `shouldBe` Just (Month 10)
        it "parses n to 11" $ do parseMonth "n" `shouldBe` Just (Month 11)
        it "parses d to 12" $ do parseMonth "d" `shouldBe` Just (Month 12)
    describe "show" $ do
        it "shows (Month 1) to January" $ do show (Month 1) `shouldBe` "January"
        it "shows (Month 2) to February" $ do show (Month 2) `shouldBe` "February"
        it "shows (Month 3) to March" $ do show (Month 3) `shouldBe` "March"
        it "shows (Month 4) to April" $ do show (Month 4) `shouldBe` "April"
        it "shows (Month 5) to May" $ do show (Month 5) `shouldBe` "May"
        it "shows (Month 6) to June" $ do show (Month 6) `shouldBe` "June"
        it "shows (Month 7) to July" $ do show (Month 7) `shouldBe` "July"
        it "shows (Month 8) to August" $ do show (Month 8) `shouldBe` "August"
        it "shows (Month 9) to September" $ do show (Month 9) `shouldBe` "September"
        it "shows (Month 10) to October" $ do show (Month 10) `shouldBe` "October"
        it "shows (Month 11) to November" $ do show (Month 11) `shouldBe` "November"
        it "shows (Month 12) to December" $ do show (Month 12) `shouldBe` "December"
