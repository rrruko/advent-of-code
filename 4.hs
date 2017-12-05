#!/usr/bin/env stack
-- stack --resolver lts-9.14 script

import Data.List

valid :: String -> Bool
valid = 
    all ((==1) . length) -- Each group should have only one member
    . group -- Group adjacent identical words
    . sort  -- Sort the words
    . words -- String -> List of words

valid2 :: String -> Bool
valid2 = 
    all ((==1) . length) -- Each group should have only one member
    . group -- Group adjacent identical words
    . sort -- Sort the sorted words
    . map sort -- Sort each word's letters
    . words -- String -> List of words

main = do
    ls <- lines <$> readFile "4.txt"
    print . length . filter valid  $ ls
    print . length . filter valid2 $ ls
