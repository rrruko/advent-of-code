import Data.List ((\\))
import Data.Map ((!), Map)
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

getNeighbors :: Map Int [Int] -> Set Int -> Int -> Set Int
getNeighbors graph visited v =
    let neighbors = (graph ! v) \\ toList visited
        newVisited = visited <> fromList neighbors
    in  S.unions $ singleton v : map (getNeighbors graph newVisited) neighbors

countGroups :: Map Int [Int] -> Int
countGroups graph = go graph 0
    where go graph count
              | M.null graph = count
              | otherwise =
                  let arbitrary = fst . head $ M.toList graph
                      group = getNeighbors graph empty arbitrary
                  in  go (graph `M.difference` setToMap group) (count + 1)

setToMap :: Set Int -> Map Int ()
setToMap = M.fromSet (const ())
