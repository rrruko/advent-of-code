import Data.Char (ord)
import Data.List (elemIndex)
import Data.Vector ((!), (//), Vector)
import qualified Data.Vector as V

data State = State
    (Vector Int) -- ^ Registers
    Int -- ^ Sound
    Int -- ^ Recover
    Int -- ^ Program counter
    deriving Show

initState :: State
initState = State (V.replicate 26 0) 0 0 0

index :: String -> Int
index c = ord (head c) - ord 'a'

op :: [String] -> State -> State
op stmt (State regs snd rcv pc) =
  State
    (case stmt of
        ["set", x, y] -> set regs x (const $ val y)
        ["add", x, y] -> set regs x (+       val y)
        ["mul", x, y] -> set regs x (*       val y)
        ["mod", x, y] -> set regs x (`mod`   val y)
        _             -> regs)
    (case stmt of
        ["snd", x] -> val x
        _          -> snd)
    (case stmt of
        ["rcv", x] | val x > 0 -> snd
        _                      -> rcv)
    (case stmt of
        ["jgz", x, y] | val x > 0 -> pc + val y
        _                         -> pc + 1)
    where
        val :: String -> Int
        val c
            | ix <- index c, ix >= 0, ix < 26 = regs ! ix
            | otherwise = read c
        set :: Vector Int -> String -> (Int -> Int) -> Vector Int
        set regs tgt f = regs // [(index tgt, f (val tgt))]

main :: IO ()
main = do
    file <- lines <$> readFile "18.txt"
    putStrLn $ "Part 1: " ++ show (compute file initState)

compute :: [String] -> State -> Int
compute instructions state@(State regs snd rcv pc)
    | rcv /= 0  = rcv
    | otherwise = compute instructions (op (words $ instructions !! pc) state)
