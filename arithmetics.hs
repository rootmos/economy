module Arithmetics (arithmetics) where

import Test.Hspec
import Text.Parsec
import Data.Bifunctor

type Number = Int

arithmetics :: String -> Either String Number
arithmetics = second doArithmetics . parseArithmetics

data Expr = Literal Number | Plus Expr Expr
    deriving (Show, Eq)

arithmeticsParser :: Parsec String st Expr
arithmeticsParser = try aPlusb <|> literal

aPlusb :: Parsec String st Expr
aPlusb = do
    a <- literal
    _ <- plus
    b <- literal
    return $ Plus a b

literal :: Parsec String st Expr
literal = do
    skipMany space
    n <- many digit
    return $ Literal (read n)

plus :: Parsec String st Char
plus = do
    skipMany space
    char '+'

parseArithmetics :: String -> Either String Expr
parseArithmetics input = first show $ parse arithmeticsParser "parsing arithmetics" input

doArithmetics :: Expr -> Number
doArithmetics (Literal n) = n
doArithmetics (Plus a b) = doArithmetics a + doArithmetics b

main :: IO ()
main = hspec $ do
    describe "doArithmetics" $ do
        it "calculates 7 to 7" $ do
            doArithmetics (Literal 7) `shouldBe` 7
        it "calculates 1+2 to 3" $ do
            doArithmetics (Plus (Literal 1) (Literal 2)) `shouldBe` 3
        it "calculates (1+2)+3 to 6" $ do
            doArithmetics (Plus (Plus (Literal 1) (Literal 2)) (Literal 3)) `shouldBe` 6

    describe "parseArithmetics" $ do
        it "parses a lone digit" $ do
            parseArithmetics "7" `shouldBe` Right (Literal 7)
        it "parses a number" $ do
            parseArithmetics "142" `shouldBe` Right (Literal 142)
        it "ignores whitespace before a number" $ do
            parseArithmetics " \t 79\n" `shouldBe` Right (Literal 79)
        it "parses 1+2" $ do
            parseArithmetics "1+2" `shouldBe` Right (Plus (Literal 1) (Literal 2))
        it "ignores whitespace before +" $ do
            parseArithmetics "1 \t  \n  +2" `shouldBe` Right (Plus (Literal 1) (Literal 2))

