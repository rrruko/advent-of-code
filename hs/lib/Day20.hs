{-# LANGUAGE OverloadedStrings #-}

import Control.Lens
import Control.Monad
import Data.Function
import Data.Functor
import Data.List
import Data.Maybe
import Data.Void
import Linear
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal, signed)

type Parser = Parsec Void String

data Particle = Particle {
    position :: V3 Int,
    velocity :: V3 Int,
    acceleration :: V3 Int
} deriving (Eq, Show)

parseParticle :: Parser Particle
parseParticle = Particle <$>
    ("p=" *> parseVector <* ", ") <*>
    ("v=" *> parseVector <* ", ") <*>
    ("a=" *> parseVector)

parseVector :: Parser (V3 Int)
parseVector = V3 <$> ("<" *> int <* ",") <*> (int <* ",") <*> (int <* ">")

int :: Parser Int
int = signed space decimal

closestToOrigin :: [(Int, Particle)] -> (Int, Particle)
closestToOrigin = minimumBy (compare `on` quadrance . position . snd)

step :: (Int, Particle) -> (Int, Particle)
step (ix, Particle pos vel acc) = (ix, Particle (pos + vel) (vel + acc) acc)

part1 :: [Particle] -> Int
part1 particles = fst $ closestToOrigin
    [iterate step p !! 1000 | p <- zip [0..] particles]

toAxis :: Particle -> Getting c (V3 Int) c -> (c, c, c)
toAxis (Particle p v a) _lens = (p^._lens, v^._lens, a^._lens)

isqrt :: Integral a => a -> Maybe a
isqrt x
    | sq * sq == x = Just sq
    | otherwise    = Nothing
    where sq = floor . sqrt $ fromIntegral x

-- Divide two integers, but only if they're divisible
(/?) :: Integral a => a -> a -> Maybe a
a /? b
    | rem a b == 0 = Just (div a b)
    | otherwise    = Nothing

data Roots = Some [Int] | Everywhere
    deriving (Eq, Show)

(*^*) :: Roots -> Roots -> Roots
Some xs    *^* Some ys    = Some (xs `intersect` ys)
Some xs    *^* Everywhere = Some xs
Everywhere *^* Some ys    = Some ys
Everywhere *^* Everywhere = Everywhere

empty :: Roots -> Bool
empty Everywhere = False
empty (Some xs)  = null xs

-- dioQuadratic a b c tries to solve the equation a²x + bx + c = 0
dioQuadratic :: Int -> Int -> Int -> Roots
dioQuadratic 0 0 0 = Everywhere
dioQuadratic 0 0 c = Some []
dioQuadratic 0 b c = Some $ maybeToList ((-c) /? b)
dioQuadratic a b c =
    case isqrt (b*b - 4*a*c) of
        Just s  -> Some $ catMaybes [((-b) + s) /? (2*a), ((-b) - s) /? (2*a)]
        Nothing -> Some []

-- The times at which the projections of two 3D particles to an axis collide.
collideAx :: (Int, Int, Int) -> (Int, Int, Int) -> Roots
collideAx (pos1, vel1, acc1) (pos2, vel2, acc2) =
    dioQuadratic (acc2 - acc1) (vel2 - vel1) (pos2 - pos1)

collideOn :: Particle -> Particle -> Getting Int (V3 Int) Int -> Roots
collideOn p1 p2 _lens = collideAx (toAxis p1 _lens) (toAxis p2 _lens)

-- Solve the quadratic equation for the difference in particle position on each
-- axis. Iff they collide on all axes at the same t, then they really collide.
collides :: Particle -> Particle -> Bool
collides p q = not $ empty (txs *^* tys *^* tzs)
    where V3 txs tys tzs = collideOn p q <$> V3 _x _y _z

-- Subset of particles that eventually collide
findCollisions :: [Particle] -> [Particle]
findCollisions xs = [x | x <- xs, collidesAny x xs]
    where collidesAny x xs = any (\e -> e /= x && collides e x) xs

part2 :: [Particle] -> Int
part2 particles = length particles - length (findCollisions particles)

readParticles :: IO [Particle]
readParticles = do
    file <- lines <$> readFile "20.txt"
    pure $ mapMaybe (parseMaybe parseParticle) file

main :: IO ()
main = do
    particles <- readParticles
    putStrLn $ "Part 1: " ++ show (part1 particles)
    putStrLn $ "Part 2: " ++ show (part2 particles)
