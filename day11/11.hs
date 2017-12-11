import Data.Char (isAlpha)
import Data.List (scanl')
import Data.List.Split (splitOn)
import Data.Ratio ((%), Ratio)

toDisp :: String -> (Ratio Int, Ratio Int)
toDisp "n"  = (   0,    1)
toDisp "ne" = ( 1%2,  1%2)
toDisp "se" = ( 1%2, -1%2)
toDisp "s"  = (   0,   -1)
toDisp "sw" = (-1%2, -1%2)
toDisp "nw" = (-1%2,  1%2)

add :: Num a => (a, a) -> (a, a) -> (a, a)
add (a, a') (b, b') = (a + b, a' + b')

taxi :: Num a => (a, a) -> a
taxi (a, b) = abs a + abs b

main :: IO ()
main = do
    file <- readFile "11.txt"
    let moves = filter (all isAlpha) $ splitOn "," file
        disps = map toDisp moves
        ans = taxi <$> scanl' add (0, 0) disps
    putStrLn $ "Part 1: " ++ show (last ans)
    putStrLn $ "Part 2: " ++ show (maximum ans)
