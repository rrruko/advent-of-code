#!/bin/env stack
-- stack script --resolver lts-9.17

import Control.Arrow ((&&&))
import Data.Char (isAlpha)
import Data.Function (on)
import Data.List
import Data.Maybe (catMaybes, fromJust, isJust)
import GHC.Exts (the)

main = do
    file <- readFile "7.txt"
    putStrLn $ part1 file
    putStrLn $ part2 file

type Name = String
type Weight = Int
data Program = Program Name Weight [Program] deriving Show
-- ^ A program has a name, a weight, and some programs stacked on top of it

getName (Program name _ _) = name
getWeight (Program _ weight _)  = weight
getChildren (Program _ _ children) = children

-- Create a full program tree given the lines and the root
readProgram :: [String] -> String -> Program
readProgram all this =
    case words this of
        [name, weight] -> Program name (read weight) []
        (name:weight:rest) ->
            let rest' = map (filter isAlpha) rest
            in  Program
                    name
                    (read weight)
                    (map (readProgram all) . catMaybes $ map (findLine all) rest')

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
-- siblings later. (TODO: Maybe just return the total weights of the siblings here,
-- instead of the parent?)
findUnbalanced :: Program -> (Program, Maybe Program)
findUnbalanced program = go program program
    where
        go parent (Program _ _ []) = (parent, Nothing)
        go parent this@(Program _ _ programs) =
            case findDifferentBy totalWeight programs of
                Nothing   -> (parent, Just this)
                Just diff -> go this diff

-- An element of a list that is different from every other element
findDifferentBy :: Ord b => (a -> b) -> [a] -> Maybe a
findDifferentBy f = fmap head
    . find (\x -> length x == 1)
    . groupBy ((==) `on` f)
    . sortOn f

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
