#!/usr/bin/env stack
-- stack --resolver lts-9.14 script --package hspec

import Control.Monad (guard)
import Data.Char
import Test.Hspec

-- This just reads a string as a list of digits.
-- We want to call read on each char in the input string, but since read
-- requires a string, we use pure to turn the chars into strings.
toDigits :: String -> [Int]
toDigits str = map (read . pure) $ filter isDigit str

-- Pair up every digit in a digit list with another element according to the
-- supplied function, and then add up all the digits that got paired with a
-- duplicate.
sumRepeatsWith :: ([Int] -> [Int]) -> [Int] -> Int
sumRepeatsWith permute digits =
    let isRepeat (x, y) = x == y
        pairs = zip digits (permute digits)
    in  sum . map fst $ filter isRepeat pairs

-- shiftLeft just shifts a digit list left by 1, sending the first one to the
-- end.
captcha :: [Int] -> Int
captcha = sumRepeatsWith shiftLeft
    where shiftLeft (x:xs) = xs ++ [x]

-- shiftHalf swaps the first and second halves of a digit list.
captcha2 :: [Int] -> Int
captcha2 = sumRepeatsWith shiftHalf
    where shiftHalf xs =
              let half = length xs `div` 2
              in  drop half xs ++ take half xs

-- These test cases are from the assignment page.
test :: IO ()
test = hspec $ do
    describe "captcha" $ do
        it "works" $ do
            captcha (toDigits "1122")     `shouldBe` 3
            captcha (toDigits "1111")     `shouldBe` 4
            captcha (toDigits "1234")     `shouldBe` 0
            captcha (toDigits "91212129") `shouldBe` 9
    describe "captcha2" $ do
        it "works" $ do
            captcha2 (toDigits "1212")     `shouldBe` 6
            captcha2 (toDigits "1221")     `shouldBe` 0
            captcha2 (toDigits "123425")   `shouldBe` 4
            captcha2 (toDigits "123123")   `shouldBe` 12
            captcha2 (toDigits "12131415") `shouldBe` 4

main :: IO ()
main = do
    test
    bign <- toDigits <$> readFile "1.txt"
    print $ captcha  bign
    print $ captcha2 bign
