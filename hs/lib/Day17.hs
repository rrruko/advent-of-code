module Day17 (
    main
    ) where

import Data.List
import Data.Maybe
import qualified Data.Sequence as S

step :: (S.Seq Int, Int, Int, Int) -> (S.Seq Int, Int, Int, Int)
step (buffer, curPos, val, stepSize) =
    let insertPos = (curPos + stepSize) `mod` length buffer
        newBuffer = S.insertAt insertPos val buffer
        newPos = insertPos + 1
    in  (newBuffer, newPos, val + 1, stepSize)

main :: IO ()
main = do
    stepSize <- read <$> readFile "17.txt"
    putStrLn $ "Part 1: " ++ show (part1 stepSize)

part1 :: Int -> Int
part1 stepSize = flip S.index 1
    . S.dropWhileL (/=2017)
    . (\(a,b,c,d) -> a)
    $ iterate' step (S.fromList [0], 0, 1, stepSize) 2017

iterate' :: (a -> a) -> a -> Int -> a
iterate' f x 0 = x
iterate' f x n = x `seq` iterate' f (f x) (n-1)
