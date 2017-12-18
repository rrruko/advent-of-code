module Day12 (
    getNeighbors,
    countGroups
    ) where

import Data.List ((\\))
import Data.Map ((!), Map, difference, fromSet)
import qualified Data.Map as M
import Data.Semigroup
import Data.Set (Set, empty, fromList, toList, singleton)
import qualified Data.Set as S

parseLine :: String -> (Int, [Int])
parseLine line = (read this, neighbors)
    where (this:arr:rest) = words line
          neighbors = read $ "[" ++ unwords rest ++ "]"

main :: IO ()
main = do
    file <- readFile "12.txt"
    let graph = M.fromList $ parseLine <$> lines file
    putStrLn $ "Part 1: " <> show (part1 graph)
    putStrLn $ "Part 2: " <> show (part2 graph)

part1 :: Map Int [Int] -> Int
part1 graph = S.size $ getNeighbors graph empty 0

part2 :: Map Int [Int] -> Int
part2 = countGroups

getNeighbors :: (Eq a, Ord a) => Map a [a] -> Set a -> a -> Set a
getNeighbors graph visited v =
    let neighbors = (graph ! v) \\ toList visited
        newVisited = visited <> fromList neighbors
    in  S.unions $ singleton v : map (getNeighbors graph newVisited) neighbors

countGroups :: Ord a => Map a [a] -> Int
countGroups graph =
    let go graph count
            | M.null graph = count
            | otherwise =
                let arbitrary = fst . head $ M.toList graph
                    group = getNeighbors graph empty arbitrary
                in  go (graph `difference` setToMap group) (count + 1)
    in go graph 0

setToMap :: Set a -> Map a ()
setToMap = fromSet (const ())
