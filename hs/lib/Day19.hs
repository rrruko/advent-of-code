module Day19 where

import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Linear

getNeighbors :: V2 Int -> [V2 Int]
getNeighbors (V2 x y) = [V2 (x-1) y, V2 (x+1) y, V2 x (y-1), V2 x (y+1)]

mkDiagram :: String -> Map (V2 Int) Char
mkDiagram file = M.fromList $ zip [V2 x y | x <- [0..width], y <- [0..height]] file
    where width = fromJust (elemIndex '\n' file) + 1
          height = length file `div` width

lookupNeighbors :: Map (V2 Int) Char -> V2 Int -> [(V2 Int, Char)]
lookupNeighbors diagram pos =
    let neighbors = getNeighbors pos
    in  [(a, b) | (a, Just b) <- zip neighbors (map (`M.lookup` diagram) neighbors)]

part1 :: Map (V2 Int) Char -> V2 Int -> String
part1 diagram startPos = go diagram (V2 0 1) startPos ""
    where go diagram dir pos str =
              case M.lookup pos diagram of
	          Nothing  -> str
		  Just ' ' -> str
		  Just '+' -> undefined -- turn here
	          Just c -> go diagram dir (pos + dir)
		               (if isAlpha c then c:str else str)

main :: IO ()
main = do
    file <- readFile "19.txt"
    let diagram = mkDiagram file
        Just startPos = elemIndex '|' file
    putStrLn $ "Part 1: " ++ part1 diagram (V2 (startPos `mod` undefined) (startPos `div` undefined))
