import Data.Char (isAlphaNum)
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Semigroup

type Layer = (Int, Int)

-- "n: m" -> (n, m)
parseLine :: String -> Layer
parseLine line =
    let [depth, range] = map (filter isAlphaNum) $ words line
    in  (read depth, read range)

-- Whether you will be caught by the layer if you wait `delay` picoseconds
caught :: Int -> Layer -> Bool
caught delay (depth, range)
    | range < 2 = True
    | otherwise = (depth + delay) `mod` (2 * range - 2) == 0

severity :: Layer -> Int
severity (depth, range) = depth * range

scoreTrip :: [Layer] -> Int -> Int
scoreTrip layers delay = sum . map severity $ filter (caught delay) layers

safeTrip :: [Layer] -> Int -> Bool
safeTrip layers delay = all (not . caught delay) layers

main :: IO ()
main = do
    layers <- map parseLine . lines <$> readFile "13.txt"
    putStrLn $ "Part 1: " <> show (part1 layers)
    putStrLn $ "Part 2: " <> show (part2 layers)

part1 :: [Layer] -> Int
part1 layers = scoreTrip layers 0

-- It's okay to use fromJust here because find never returns Nothing on an
-- infinite list
part2 :: [Layer] -> Int
part2 layers = fromJust $ find (safeTrip layers) [0..]
