#!/usr/bin/env stack
-- stack script --resolver lts-9.18

module Day15 where

import Data.Bits
import Data.Semigroup

divisibleBy :: Int -> Int -> Bool
n `divisibleBy` m = n `mod` m == 0

a, b, a', b' :: Int -> [Int]
a = iterate (\x -> (x * 16807) `mod` 2147483647)
b = iterate (\x -> (x * 48271) `mod` 2147483647)
a' = filter (`divisibleBy` 4) . a
b' = filter (`divisibleBy` 8) . a

match :: Int -> Int -> Bool
match x y = xor x y .&. 0xffff == 0

part1, part2 :: Int -> Int -> Int
part1 aInit bInit = length . filter (uncurry match) 
    . take (40 * 10^6) $ zip (a  aInit) (b  bInit)
part2 aInit bInit = length . filter (uncurry match) 
    . take (5  * 10^6) $ zip (a' aInit) (b' bInit)

main :: IO ()
main = do
    putStrLn $ "Part 1: " <> show (part1 883 879)
    putStrLn $ "Part 2: " <> show (part2 883 879)
