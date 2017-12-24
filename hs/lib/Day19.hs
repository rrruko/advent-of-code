module Day19 where

import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x,y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

diagram :: String -> Map (Int, Int) Char
diagram file = M.fromList $ zip [(x, y) | x <- [0..width], y <- [0..height]] file
    where width = fromJust (elemIndex '\n' file) + 1
          height = length file `div` width

main :: IO ()
main = do
    file <- readFile "19.txt"
    putStrLn . map snd . M.toList $ diagram file
