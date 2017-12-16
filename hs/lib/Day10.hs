module Day10
    ( def
    , getKnot
    , pad
    , part1
    , part2
    , rev
    , reverseUpTo
    , shift
    ) where

import Data.Bits (xor)
import Data.Char (isSpace, ord)
import Data.List (foldl')
import Data.List.Split (chunksOf)
import Data.Semigroup
import Data.Vector ((!), Vector, fromList, singleton)
import qualified Data.Vector as V
import Numeric (showHex)

shift :: Int -> Vector a -> Vector a
shift n xs
    | n > 0 =
        let y = singleton (V.last xs) <> V.init xs
        in V.last y `seq` shift (n-1) y
    | n < 0 =
        let y = V.tail xs <> singleton (V.head xs)
        in V.last y `seq` shift (n+1) y
    | otherwise = xs

reverseUpTo :: Int -> Vector a -> Vector a
reverseUpTo len xs = V.reverse (V.take len xs) <> V.drop len xs

rev :: Int -> Int -> Vector a -> Vector a
rev index len = shift index . reverseUpTo len . shift (-index)
 
data State = State {
    knot :: Vector Int,
    skipSize :: Int,
    pos :: Int
} deriving Show

getKnot :: State -> Vector Int
getKnot (State k s p) = k

def :: State
def = State (fromList [0..255]) 0 0

part1 :: Vector Int -> State -> State
part1 lengths initState = foldl' 
    (\(State knot skipSize pos) len -> let r = rev pos len knot in r `seq`
        State r (skipSize + 1) ((pos + len + skipSize) `mod` length knot))
    initState
    lengths

part2 :: String -> State -> String
part2 lengths initKnot =
    let lengths' = fromList (map ord lengths) <> fromList [17, 31, 73, 47, 23]
        sparseHash = getKnot $ iterate (part1 lengths') initKnot !! 64
        foldXor = foldr xor 0
        denseHash = map foldXor $ chunksOf 16 (V.toList sparseHash)
    in  concat [pad 2 '0' $ showHex x "" | x <- denseHash]

pad :: Int -> Char -> String -> String
pad len c str = replicate (len - length str) c ++ str

main :: IO ()
main = do
    file <- filter (not . isSpace) <$> readFile "10.txt"
    let lengths = read $ "[" ++ file ++ "]"
    print . (\x -> x!0 * x!1) . getKnot $ part1 lengths def
    putStrLn $ part2 file def
