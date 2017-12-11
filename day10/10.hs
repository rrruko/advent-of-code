import Data.Bits (xor)
import Data.Char (isSpace, ord)
import Data.List (foldl')
import Data.List.Split (chunksOf)
import Numeric (showHex)

shift :: Int -> [a] -> [a]
shift n xs
    | n > 0 = let y = (last xs : init xs)    in last y `seq` shift (n-1) y
    | n < 0 = let y = (tail xs ++ [head xs]) in shift (n+1) y
    | otherwise = xs

reverseUpTo :: Int -> [a] -> [a]
reverseUpTo len xs = reverse (take len xs) ++ drop len xs

rev :: Int -> Int -> [a] -> [a]
rev index len = shift index . reverseUpTo len . shift (-index)
 
data State = State {
    knot :: [Int],
    skipSize :: Int,
    pos :: Int
}

getKnot (State k s p) = k

part1 :: [Int] -> State -> State
part1 lengths initState = foldl' 
    (\(State knot skipSize pos) len -> let r = rev pos len knot in r `seq`
        State r (skipSize + 1) ((pos + len + skipSize) `mod` length knot))
    initState
    lengths

part2 :: String -> State -> String
part2 lengths initKnot =
    let lengths' = map ord lengths ++ [17, 31, 73, 47, 23]
        sparseHash = getKnot $ iterate (part1 lengths') initKnot !! 64
        foldXor = foldr xor 0
        denseHash = map foldXor $ chunksOf 16 sparseHash
    in  concat [showHex x "" | x <- denseHash]

main :: IO ()
main = do
    file <- filter (not . isSpace) <$> readFile "10.txt"
    let lengths = read $ "[" ++ file ++ "]"
    print . (\x -> x!!0 * x!!1) . getKnot $ part1 lengths (State [0..255] 0 0)
    putStrLn $ part2 file (State [0..255] 0 0)
