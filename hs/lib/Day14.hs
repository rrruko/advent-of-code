module Day14
    (answer
    ) where

import Day10 (def, pad, part2)

import Data.Bool (bool)
import Data.List (intercalate)
import Numeric

hexToBin :: String -> String
hexToBin n = showIntAtBase 2 (bool '#' '.' . (==0)) (fst . head $ readHex n) ""

showHash :: String -> String
showHash str = part2 str def >>= (pad 4 '.' . hexToBin . pure)

answer :: String -> String
answer str = intercalate "\n" $ map (showHash . (str ++) . show) [0..127]
