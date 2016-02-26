module Arithmetics (arithmetics) where

import Test.Hspec
import Text.Parsec
import Data.Bifunctor

type Number = Int

arithmetics :: String -> Either String Number
arithmetics = second doArithmetics . parseArithmetics

data Expr = Literal Number | Plus Expr Expr | Minus Expr Expr | Mult Expr Expr
    deriving (Show, Eq)

arithmeticsParser :: Parsec String st Expr
arithmeticsParser = chainl literal (try binaryOps) (Literal 0)

literal :: Parsec String st Expr
literal = do
    skipMany space
    sign <- option 1 (char '-' >> return (-1))
    n <- many1 digit
    return $ Literal (sign * (read n))

binaryOps :: Parsec String st (Expr -> Expr -> Expr)
binaryOps = skipMany space >> choice [plus, minus, mult]
    where
        plus = char '+' >> return Plus
        minus = char '-' >> return Minus
        mult = char '*' >> return Mult

parseArithmetics :: String -> Either String Expr
parseArithmetics = first show . parse arithmeticsParser "parsing arithmetics"

doArithmetics :: Expr -> Number
doArithmetics (Literal n) = n
doArithmetics (Plus a b) = doArithmetics a + doArithmetics b
doArithmetics (Minus a b) = doArithmetics a - doArithmetics b
doArithmetics (Mult a b) = doArithmetics a * doArithmetics b

main :: IO ()
main = hspec $ do
    describe "doArithmetics" $ do
        it "calculates 7 to 7" $ do
            doArithmetics (Literal 7) `shouldBe` 7
        it "calculates 1+2 to 3" $ do
            doArithmetics (Plus (Literal 1) (Literal 2)) `shouldBe` 3
        it "calculates (1+2)+3 to 6" $ do
            doArithmetics (Plus (Plus (Literal 1) (Literal 2)) (Literal 3)) `shouldBe` 6
        it "calculates 1-2 to -1" $ do
            doArithmetics (Minus (Literal 1) (Literal 2)) `shouldBe` -1
        it "calculates (4-3)-(1-2) to 2" $ do
            doArithmetics (Minus (Minus (Literal 4) (Literal 3)) (Minus (Literal 1) (Literal 2))) `shouldBe` 2
        it "calculates 4*3" $ do
            doArithmetics (Mult (Literal 4) (Literal 3)) `shouldBe` 12
        it "calculates 2*(1-3)" $ do
            doArithmetics (Mult (Literal 2) (Minus (Literal 1) (Literal 3))) `shouldBe` -4

    describe "parseArithmetics" $ do
        it "parses a lone digit" $ do
            parseArithmetics "7" `shouldBe` Right (Literal 7)
        it "parses a number" $ do
            parseArithmetics "142" `shouldBe` Right (Literal 142)
        it "parses a negative number" $ do
            parseArithmetics "-17" `shouldBe` Right (Literal (-17))
        it "ignores whitespace before a number" $ do
            parseArithmetics " \t 79" `shouldBe` Right (Literal 79)
        it "ignores whitespace after a number" $ do
            parseArithmetics "79 " `shouldBe` Right (Literal 79)
        it "parses 1+2" $ do
            parseArithmetics "1+2" `shouldBe` Right (Plus (Literal 1) (Literal 2))
        it "ignores whitespace before +" $ do
            parseArithmetics "1 \t  \n  +2" `shouldBe` Right (Plus (Literal 1) (Literal 2))
        it "parses 1+2+3 left-associatively" $ do
            parseArithmetics "1+2+3" `shouldBe` Right (Plus (Plus (Literal 1) (Literal 2)) (Literal 3))

        it "parses 1-2" $ do
            parseArithmetics "1-2" `shouldBe` Right (Minus (Literal 1) (Literal 2))
        it "parses 1--2" $ do
            parseArithmetics "1--2" `shouldBe` Right (Minus (Literal 1) (Literal (-2)))
        it "parses 1-2+3 left-associatively" $ do
            parseArithmetics "1-2+3" `shouldBe` Right (Plus (Minus (Literal 1) (Literal 2)) (Literal 3))
        it "parses 1+2-3 left-associatively" $ do
            parseArithmetics "1+2-3" `shouldBe` Right (Minus (Plus (Literal 1) (Literal 2)) (Literal 3))

        it "parses 3*4" $ do
            parseArithmetics "3*4" `shouldBe` Right (Mult (Literal 3) (Literal 4))
        it "parses 1+2*3 left-associatively" $ do
            parseArithmetics "1+2*3" `shouldBe` Right (Mult (Plus (Literal 1) (Literal 2)) (Literal 3))
        it "parses 1*2-3 left-associatively" $ do
            parseArithmetics "1*2-3" `shouldBe` Right (Minus (Mult (Literal 1) (Literal 2)) (Literal 3))