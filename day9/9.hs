#!/usr/bin/env stack
-- stack script --resolver lts-9.14

applyBangs :: String -> String
applyBangs str = go ("", str)
    where
        go (l, '!':_:tl) = go (l, tl)
        go (l, []) = reverse l
        go (l, r) = go (head r:l, tail r)

cleanGarbage :: String -> String
cleanGarbage str = go ("", str)
    where
        go (l, []) = reverse l
        go (l, '<':'>':tl) = go (l, tl)
        go (l, '<':_:tl) = go (l, '<':tl)
        go (l, c:tl) = go (c:l, tl)

countingClean :: String -> Int
countingClean str = go 0 ("", str)
    where
        go count (l, []) = count
        go count (l, '<':'>':tl) = go count (l, tl)
        go count (l, '<':_:tl) = go (count + 1) (l, '<':tl)
        go count (l, c:tl) = go count (c:l, tl)

sumGroups :: String -> Int
sumGroups str = go 0 1 str
    where
        go sum level ('{':rest) = go (sum + level) (level + 1) rest
        go sum level ('}':rest) = go sum (level - 1) rest
        go sum level (_:rest) = go sum level rest
        go sum level [] = sum

part1 :: String -> Int
part1 = sumGroups . cleanGarbage . applyBangs

part2 :: String -> Int
part2 = countingClean . applyBangs

main :: IO ()
main = do
    input <- readFile "9.txt"
    putStrLn $ "Part 1: " ++ show (part1 input)
    putStrLn $ "Part 2: " ++ show (part2 input)
