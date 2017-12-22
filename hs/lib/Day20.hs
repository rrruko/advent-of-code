{-# LANGUAGE OverloadedStrings #-}

import Control.Lens
import Data.Function
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

main :: IO ()
main = do
    file <- lines <$> readFile "20.txt"
    let particles = mapMaybe (parseMaybe parseParticle) file
    print . fst . closestToOrigin $ map (\p -> iterate step p !! 1000) $ zip [0..] particles
    print . length $ findCollisions particles

collides :: Particle -> Particle -> Bool
collides p q = 
    case collideOn p q <$> V3 _x _y _z of
        V3 (Just a) (Just b) (Just c) -> a == b && b == c
        _ -> False

findCollisions :: [Particle] -> [Particle]
findCollisions xs = [x | x <- xs, collidesAny x xs]
    where collidesAny x xs = any (\e -> e /= x && collides e x) xs

toAxis :: Particle -> Getting c (V3 Int) c -> (c, c, c)
toAxis (Particle p v a) _lens = (p^._lens, v^._lens, a^._lens)

collideOn :: Particle -> Particle -> Getting Int (V3 Int) Int -> Maybe Int
collideOn p1 p2 _lens = collideAx (toAxis p1 _lens) (toAxis p2 _lens)

-- The t at which the projections of two 3D particles to an axis collide.
collideAx :: (Int, Int, Int) -> (Int, Int, Int) -> Maybe Int
collideAx (pos1, vel1, acc1) (pos2, vel2, acc2) =
    let a = acc2 - acc1
        b = vel2 - vel1
        c = pos2 - pos1
    in  case isqrt(b*b - 4*a*c) of
            Just s | a /= 0 ->
                if ((-b) + s) `divisibleBy` (2*a) then
                    Just $ ((-b) + s) `div` (2*a)
                else if ((-b) - s) `divisibleBy` (2*a) then
                    Just $ ((-b) - s) `div` (2*a)
                else
                    Nothing
            Just s | b /= 0 ->
                if (-c) `divisibleBy` b then
                    Just $ (-c) `div` b
                else
                    Nothing
            Just s -> if c == 0 then Just 0 else Nothing
            Nothing -> Nothing

isqrt :: Integral a => a -> Maybe a
isqrt x
    | sq * sq == x = Just sq
    | otherwise    = Nothing
    where sq = floor . sqrt $ fromIntegral x

divisibleBy :: Integral a => a -> a -> Bool
x `divisibleBy` y = rem x y == 0
