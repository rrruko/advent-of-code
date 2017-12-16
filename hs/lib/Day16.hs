module Day16 where

import Data.Semigroup
import Data.Vector ((!), (//), Vector)
import qualified Data.Vector as V
import Data.Void
import Text.Megaparsec

type Permutation = Vector Char -> Vector Char

spin :: Int -> Permutation
spin n str =
    let len = V.length str
        res = V.drop (len - n) str <> V.take (len - n) str
    in  V.force res

exchange :: Int -> Int -> Permutation
exchange n m str =
    let a = str ! n
        b = str ! m
    in  str // [(m, a), (n, b)]

partner :: Char -> Char -> Permutation
partner a b str =
    let Just n = V.elemIndex a str
        Just m = V.elemIndex b str
    in  str // [(m, a), (n, b)]

type Parser = Parsec () String

int :: Parser Int
int = read <$> many digitChar

-- Apply the parsers for each kind of move until all the input is consumed, then
-- compose all the results.
parseExpr :: Parser Permutation
parseExpr = foldr1 (flip (.)) <$> 
    many (parseSpin <|> parseExchange <|> parsePartner)

parseSpin :: Parser Permutation
parseSpin = char 's' *> (spin <$> int)

parseExchange :: Parser Permutation
parseExchange = char 'x' *> (exchange <$> int <*> (char '/' *> int))

parsePartner :: Parser Permutation
parsePartner = char 'p' *> (partner <$> lowerChar <*> (char '/' *> lowerChar))

main :: IO ()
main = do
    -- We don't need the commas or newlines to parse the input
    file <- filter (`notElem` ",\n") <$> readFile "16.txt"
    let ini = V.fromList "abcdefghijklmnop"
    case parseMaybe parseExpr file of
        Just f -> do
            putStrLn $ "Part 1: " <> V.fromList (part1 f ini)
            putStrLn $ "Part 2: " <> V.fromList (part2 f ini)
        Nothing -> putStrLn "Parse failed"

-- For part 1, we just need to apply the permutations to the initial string.
part1 :: Permutation -> Vector Char -> Vector Char
part1 f ini = f ini

-- For part 2, we need to apply the permutation one billion times.
-- Fortunately, a cycle occurs before we hit one billion, so we'll get the same
-- result by just skipping all the complete cycles.
part2 :: Permutation -> Vector Char -> Vector Char
part2 f ini = iterate' f ini (1000000000 `mod` cycleLength [] f ini)

-- Apply a function to a value, remembering every intermediate result. The
-- length of the cycle is the number of iterations it takes for a result to
-- appear twice.
cycleLength :: Eq a => [a] -> (a -> a) -> a -> Int
cycleLength history f x
    | x `notElem` history = 1 + cycleLength (x:history) f (f x)
    | otherwise           = 0

-- Strict version of iterate
iterate' :: (a -> a) -> a -> Int -> a
iterate' f x 0 = x
iterate' f x n = x `seq` iterate' f (f x) (n - 1)
