#!/bin/env stack
-- stack script --resolver lts-9.17

import Data.Char (isAlpha)
import Data.Function (on)
import Data.List
import Data.Maybe (mapMaybe)
import GHC.Exts (the)

main :: IO ()
main = do
    file <- readFile "7.txt"
    putStrLn $ part1 file
    putStrLn $ part2 file

type Name = String
type Weight = Int

-- A program has a name, a weight, and some programs stacked on top of it
data Program = Program {
    getName :: Name,
    getWeight :: Weight,
    getChildren :: [Program]
} deriving Show

-- Create a full program tree given the lines and the root
readProgram :: [String] -> String -> Program
readProgram allLines this =
    case words this of
        [name, weight] -> Program name (read weight) []
        (name:weight:rest) ->
            let childNames = map (filter isAlpha) rest
                childLines = mapMaybe (findLine allLines) childNames
                children = map (readProgram allLines) childLines
            in  Program name (read weight) children

-- We can't write this using isPrefixOf because some program names are prefixes
-- of others
findLine :: [String] -> String -> Maybe String
findLine context prefix = find ((prefix ==) . head . words) context

-- The weight of a program plus those of all programs above it
totalWeight :: Program -> Weight
totalWeight (Program _ n []) = n
totalWeight (Program _ n programs) = n + sum (map totalWeight programs)

-- Find a program in the program tree that has a different total weight from its
-- siblings. We keep track of the parent so that we can get the weights of the
-- siblings later.
findUnbalanced :: Program -> (Program, Maybe Program)
findUnbalanced program = go program program
    where
        go parent (Program _ _ []) = (parent, Nothing)
        go parent this@(Program _ _ programs) =
            case misfitBy totalWeight programs of
                Nothing -> (parent, Just this)
                Just (misfit, _) -> go this misfit

-- The smallest element that is not equal to any other.
-- If there isn't one, return Nothing.
-- If there are many, return the smallest.
misfitBy :: (Eq b, Ord b) => (a -> b) -> [a] -> Maybe (a, [a])
misfitBy f xs
    | allEqual (map f xs) = Nothing
    | otherwise = uncons
        . concat
        . sortOn length
        . groupBy ((==) `on` f)
        $ sortOn f xs

allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual (x:xs) = all (== x) xs

-- The statistical mode of a list
mode :: Ord a => [a] -> a
mode = head
    . maximumBy (compare `on` length)
    . group
    . sort

-- The name of the program at the bottom
part1 :: String -> String
part1 str =
    let programNames = head . words <$> lines str
        -- ^ The first word of each line
        rhs = map (filter isAlpha) . tail . words <$> lines str
        -- ^ The rest of each line, describing the programs on top
        isBottom str = all (notElem str) rhs
        -- ^ A program is the bottom if it isn't on top of anything
    in  the $ filter isBottom programNames
        -- ^ There should be exactly one program that isBottom

-- The weight that one program needs to be changed to to balance the tree
part2 :: String -> String
part2 str =
    let Just bottomLine = findLine (lines str) (part1 str)
        -- ^ Part 1 finds the name of the program at the bottom of the tree
        stack = readProgram (lines str) bottomLine
        (parent, Just unbalanced) = findUnbalanced stack
        -- ^ Find the one program that is the wrong weight and its parent
        currWeight = getWeight unbalanced
        siblings = getChildren parent
        siblingsTotalWeight = mode $ map totalWeight siblings
    in  show $ currWeight - (totalWeight unbalanced - siblingsTotalWeight)
