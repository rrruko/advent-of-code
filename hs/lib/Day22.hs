module Day22 where

import Data.Complex
import Data.Set (Set)
import qualified Data.Set as S

instance Ord a => Ord (Complex a) where
    (a :+ b) <= (c :+ d)
        | a == c    = b <= d
	| otherwise = a <= c

type Direction = Complex Int

update :: Complex Int -> Direction -> Set (Complex Int) -> Int -> (Complex Int, Direction, Set (Complex Int), Int)
update pos dir set count
    | S.member pos set = 
        let newDir = dir !* (0 :+ (-1))
	in  (pos !+ newDir, newDir, S.delete pos set, count)
    | otherwise = 
        let newDir = dir !* (0 :+ ( 1))
	in  (pos !+ newDir, newDir, S.insert pos set, count + 1)
    where (a :+ b) !+ (c :+ d) = (a + c) :+ (b + d)
          (a :+ b) !* (c :+ d) = (a*c - b*d) :+ (a*d + b*c)

history :: Set (Complex Int) -> [(Complex Int, Direction, Set (Complex Int), Int)]
history start = iterate 
    (\(x,y,z, count) -> update x y z count) 
    (0 :+ 0, 0 :+ 1, start, 0)

parse :: String -> Set (Complex Int)
parse str = 
    let width  = length . head $ lines str
    in  S.fromList [(mod ix width - div width 2) :+ (-(div ix width - div width 2))
                   | (ix, ch) <- zip [0..] (concat $ unlines str), ch == '#']

main :: IO ()
main = do
    file <- readFile "22.txt"
    let (_,_,_,count) = history (parse file) !! 10000
    print count
