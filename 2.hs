checksum :: [[Int]] -> Int
checksum spreadsheet = sum $ map (diff . minmax) spreadsheet
    where minmax xs = (minimum xs, maximum xs)
          diff (x, y) = abs (x - y)

checksum2 :: [[Int]] -> Int
checksum2 spreadsheet = sum $ map (divide . multiples) spreadsheet
    where multiples xs = head $ filter evenlyDivides (cartProd xs)
          cartProd xs = [(x, y) | x <- xs, y <- xs]
          evenlyDivides (x, y) = x > y && mod x y == 0
          divide (x, y)
              | x > y     = div x y
              | otherwise = div y x

main :: IO ()
main = do
    file <- readFile "2.txt"
    let spreadsheet = [map read $ words line | line <- lines file]
    print $ checksum  [[5,1,9,5],[7,5,3],[2,4,6,8]] == 18
    print $ checksum  spreadsheet
    print $ checksum2 [[5,9,2,8],[9,4,7,3],[3,8,6,5]] == 9
    print $ checksum2 spreadsheet
