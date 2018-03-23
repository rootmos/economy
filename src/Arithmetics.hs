-- This file is part of economy.
--
-- economy is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- economy is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with economy.  If not, see <http://www.gnu.org/licenses/>.

module Arithmetics (arithmetics) where

import Test.Hspec
import Text.Parsec
import Data.Bifunctor
import Data.Either

type Number = Int

arithmetics :: String -> Either String Number
arithmetics = second doArithmetics . parseArithmetics

data Expr = Literal Number | Plus Expr Expr | Minus Expr Expr | Mult Expr Expr | Div Expr Expr
    deriving (Show, Eq)

arithmeticsParser :: Parsec String st Expr
arithmeticsParser = spaces >> chainl1 (literal) (binaryOps) <* eof
    where
        binaryOps = choice [plusOp, minusOp, multOp, divOp]
        plusOp = char '+' >> spaces >> return Plus
        minusOp = char '-' >> spaces >> return Minus
        multOp = char '*' >> spaces >> return Mult
        divOp = char '/' >> spaces >> return Div

literal :: Parsec String st Expr
literal = do
    sign <- option 1 (char '-' >> spaces >> return (-1))
    n <- many1 digit
    spaces
    return $ Literal (sign * (read n))

parseArithmetics :: String -> Either String Expr
parseArithmetics = first show . parse arithmeticsParser "parsing arithmetics"

doArithmetics :: Expr -> Number
doArithmetics (Literal n) = n
doArithmetics (Plus a b) = doArithmetics a + doArithmetics b
doArithmetics (Minus a b) = doArithmetics a - doArithmetics b
doArithmetics (Mult a b) = doArithmetics a * doArithmetics b
doArithmetics (Div a b) = doArithmetics a `div` doArithmetics b

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
        it "calculates 15/3" $ do
            doArithmetics (Div (Literal 15) (Literal 3)) `shouldBe` 5
        it "truncates division: 1/2 = 0" $ do
            doArithmetics (Div (Literal 1) (Literal 2)) `shouldBe` 0

    describe "parseArithmetics" $ do
        it "parses a lone digit" $ do
            parseArithmetics "7" `shouldBe` Right (Literal 7)
        it "parses a number" $ do
            parseArithmetics "142" `shouldBe` Right (Literal 142)
        it "parses a negative number" $ do
            parseArithmetics "-17" `shouldBe` Right (Literal (-17))
        it "parses a negative number (with a space): - 17" $ do
            parseArithmetics "- 17" `shouldBe` Right (Literal (-17))
        it "ignores whitespace before a number" $ do
            parseArithmetics " \t 79" `shouldBe` Right (Literal 79)
        it "ignores whitespace after a number" $ do
            parseArithmetics "79 \t \n " `shouldBe` Right (Literal 79)
        it "parses 1+2" $ do
            parseArithmetics "1+2" `shouldBe` Right (Plus (Literal 1) (Literal 2))
        it "ignores whitespace before +" $ do
            parseArithmetics "1  \n \t  +2" `shouldBe` Right (Plus (Literal 1) (Literal 2))
        it "ignores whitespace after +" $ do
            parseArithmetics "1+ \t \n 2" `shouldBe` Right (Plus (Literal 1) (Literal 2))
        it "parses 1+2+3 left-associatively" $ do
            parseArithmetics "1+2+3" `shouldBe` Right (Plus (Plus (Literal 1) (Literal 2)) (Literal 3))

        it "parses 1-2" $ do
            parseArithmetics "1-2" `shouldBe` Right (Minus (Literal 1) (Literal 2))
        it "parses 1--2" $ do
            parseArithmetics "1--2" `shouldBe` Right (Minus (Literal 1) (Literal (-2)))
        it "parses -1--2" $ do
            parseArithmetics "-1--2" `shouldBe` Right (Minus (Literal (-1)) (Literal (-2)))
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

        it "parses 1/2" $ do
            parseArithmetics "1/2" `shouldBe` Right (Div (Literal 1) (Literal 2))
        it "parses 1+2/3 left-associatively" $ do
            parseArithmetics "1+2/3" `shouldBe` Right (Div (Plus (Literal 1) (Literal 2)) (Literal 3))
        it "parses 1+2/3+4 left-associatively" $ do
            parseArithmetics "1+2/3+4" `shouldBe` Right (Plus (Div (Plus (Literal 1) (Literal 2)) (Literal 3)) (Literal 4))


        it "complains when faced with unknown characters: before a number" $ do
            parseArithmetics "a2" `shouldSatisfy` isLeft
        it "complains when faced with unknown characters: before a number (after an operator)" $ do
            parseArithmetics "1+a2" `shouldSatisfy` isLeft
        it "complains when faced with unknown characters: before an operator" $ do
            parseArithmetics "1a+2" `shouldSatisfy` isLeft
        it "complains when faced with unknown characters: after an operator" $ do
            parseArithmetics "1+a2" `shouldSatisfy` isLeft
