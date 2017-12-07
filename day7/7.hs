#!/bin/env stack
-- stack script --resolver lts-9.17

import Data.Char (isAlpha)
import GHC.Exts (the)

main = do
    file <- readFile "7.txt"
    putStrLn $ part1 file

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
