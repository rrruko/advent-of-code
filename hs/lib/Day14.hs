#!/usr/bin/env stack
-- stack --resolver lts-9.14 script --package hspec --package split

module Day14
    (main
    ) where

import Day10 (def, pad)
import qualified Day10

import Data.Bool (bool)
import Data.Vector ((!), (//), Vector)
import qualified Data.Vector as V
import Numeric

hexToBin :: String -> String
hexToBin n = showIntAtBase 2 (bool '#' '.' . (==0)) (fst . head $ readHex n) ""

showHash :: String -> String
showHash str = Day10.part2 str def >>= (pad 4 '.' . hexToBin . pure)

answer :: String -> [String]
answer str = map (showHash . ((str ++ "-") ++) . show) [0..127]

main :: IO ()
main = do
    let m = concat $ answer "hfdlxzhv"
    putStrLn $ "Part 1: " ++ show (length $ filter (=='#') m)
    putStrLn $ "Part 2: " ++ show (part2 m)

data Vector2 a = Vector2 (Vector a) Int

inBounds :: Vector2 a -> (Int, Int) -> Bool
inBounds (Vector2 v w) (x,y) =
    x >= 0 && x < w &&
    y >= 0 && y < div (length v) w

left, right, up, down :: (Int, Int) -> (Int, Int)
left  (x,y) = (x-1, y)
right (x,y) = (x+1, y)
up    (x,y) = (x, y-1)
down  (x,y) = (x, y+1)

from2d :: (Int, Int) -> Int -> Int
from2d (x,y) w = x + y * w

to2d :: Int -> Int -> (Int, Int)
to2d x w = (mod x w, div x w)

at :: Vector2 a -> (Int, Int) -> a
at (Vector2 v w) ix = v ! from2d ix w

set :: Vector2 a -> (Int, Int) -> a -> Vector2 a
set (Vector2 v w) ix n = Vector2 (v // [(from2d ix w, n)]) w

vec :: Vector2 a -> Vector a
vec (Vector2 v w) = v

floodFill :: Eq a => Vector2 a -> (Int, Int) -> a -> a -> Vector2 a
floodFill gr ix target replace
    | not (inBounds gr ix) = gr
    | target == replace    = gr
    | gr `at` ix /= target = gr
    | otherwise =
        let gr1 = set gr ix replace
            gr2 = floodFill gr1 (up ix)    target replace
            gr3 = floodFill gr2 (left ix)  target replace
            gr4 = floodFill gr3 (right ix) target replace
            gr5 = floodFill gr4 (down ix)  target replace
        in  gr5

part2 :: String -> Int
part2 str = 
    let gr = Vector2 (V.fromList str) 128
        go g n = 
            case V.findIndex (=='#') (vec g) of
                Nothing -> n
                Just ix ->
                    let g' = floodFill g (to2d ix 128) '#' '.'
                    in go g' (n+1)
    in  go gr 0
