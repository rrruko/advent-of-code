module Day21 where

import Data.List (subsequences, transpose)
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

-- Show that perms works
main :: IO ()
main = mapM_ (\m -> pretty m *> putStrLn "") $ perms initial

