module Day21 where

import Control.Applicative
import Control.Monad
import Data.List (subsequences, transpose)
import Data.List.Split
import Data.Maybe
import Data.Monoid

type Matrix = [String]

meme :: IO String
meme = readFile "21.txt"

pretty :: Matrix -> IO ()
pretty = mapM_ putStrLn

initial :: Matrix
initial =
    [".#.",
     "..#",
     "###"]

-- The 8 matrices that can be made by flipping and rotating a matrix
perms :: Matrix -> [Matrix]
perms = mapM (appEndo . foldMap Endo) $
    subsequences [reverse, map reverse, transpose]

-- Turn a line from the input file into a function that transforms a matrix
rule :: String -> Matrix -> Maybe Matrix
rule str mat
    | from `elem` perms mat = Just to
    | otherwise = Nothing
    where [from, to] = splitOn "/" <$> splitOn " => " str

-- Try applying each of a list of rules to a matrix
update :: [String] -> Matrix -> Maybe Matrix
update strs mat = foldr (<|>) Nothing $ map (\str -> rule str mat) strs

divide :: Matrix -> [Matrix]
divide = concat . fmap (chunksOf 2) . transpose . fmap (chunksOf 2)

-- Combine four quadrants into one matrix
combine :: Matrix -> Matrix -> Matrix -> Matrix -> Matrix
combine tl tr bl br = zipWith (++) tl tr ++ zipWith (++) bl br

-- Show that perms works
main :: IO ()
main = mapM_ (\m -> pretty m *> putStrLn "") $ perms initial

