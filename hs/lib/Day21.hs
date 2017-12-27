module Day21 where

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

rule :: String -> Matrix -> Maybe Matrix
rule str mat
    | from `elem` perms mat = Just to
    | otherwise = Nothing
    where [from, to] = splitOn "/" <$> splitOn " => " str

-- Show that perms works
main :: IO ()
main = mapM_ (\m -> pretty m *> putStrLn "") $ perms initial

