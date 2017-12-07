#!/bin/env stack
-- stack script --resolver lts-9.17

import Data.Char (isAlpha)
import GHC.Exts (the)

main = do
    file <- readFile "7.txt"
    putStrLn $ part1 file

part1 :: String -> String
part1 str =
    let ts = map (filter isAlpha) . tail . words <$> lines str 
        -- ^ The right hand side of the arrow on each line
        hs = head . words <$> lines str
        -- ^ The left hand side of the arrow on each line
        isBottom str = all (notElem str) ts 
        -- ^ A string is the bottom if it never appears on the right hand side of an arrow
    in  the $ filter isBottom hs
        -- ^ There should be exactly one word in hs that isBottom
