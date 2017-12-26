import Data.Char (ord)
import Data.List (elemIndex)
import Data.Vector ((!), (//), Vector)
import qualified Data.Vector as V

data State = State
    (Vector Int) -- ^ Registers
    Int -- ^ Sound
    Int -- ^ Recover
    Int -- ^ Program counter
    (Vector Int) -- ^ Send queue
    (Vector Int) -- ^ Recieve queue
    deriving Show

initState, initState' :: State
initState  = State (V.replicate 26 0)                     0 0 0 V.empty
initState' = State (V.replicate 26 0 // [(index "p", 1)]) 0 0 0 V.empty

index :: String -> Int
index c = ord (head c) - ord 'a'

op :: [String] -> State -> State
op stmt (State regs snd rcv pc sq rq) =
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
    sq
    rq
    where
        val :: String -> Int
        val c
            | ix <- index c, ix >= 0, ix < 26 = regs ! ix
            | otherwise = read c
        set :: Vector Int -> String -> (Int -> Int) -> Vector Int
        set regs tgt f = regs // [(index tgt, f (val tgt))]

op' :: [String] -> (State, [Int], [Int]) -> (State, [Int], [Int])
op' stmt st@(State regs snd rcv pc outQueue (i:inQueue)) =
  case stmt of
      ["snd", x] -> State regs                   snd rcv pc (x:outQueue) (i:inQueue)
      ["rcv", x] -> State (set regs x (const i)) snd rcv pc outQueue     inQueue
      _          -> let State regs snd rcv pc sq rq = op stmt st
                    in  State regs snd rcv pc outQueue (i:inQueue)

main :: IO ()
main = do
    file <- lines <$> readFile "18.txt"
    putStrLn $ "Part 1: " ++ show (part1 file initState)
    putStrLn $ "Part 2: " ++ show (part2 file initState initState')

part1 :: [String] -> State -> Int
part1 instructions state@(State regs snd rcv pc)
    | rcv /= 0  = rcv
    | otherwise = part1 instructions (op (words $ instructions !! pc) state)

part2 :: [String] -> State -> State -> Int
part2 instructions state1 state2 = go state1 state2 [] []
    where go s1@(State reg1 snd1 rcv1 pc1 ) s2@(State reg2 snd2 rcv2 pc2) queue1 queue2 =
            let out1 = uncons (getQueue s1)
                out2 = uncons (getQueue s2)
                s1' =
                    case fst <$> out2 of
                        Just new -> op' stmt1 s1
                        Nothing  -> s1
                s2' =
                    case fst <$> out1 of
                        Just new -> op' stmt2 s2
                        Nothing  -> s2
                queue1' = fromMaybe queue1' (snd <$> out1)
                queue2' = fromMaybe queue2' (snd <$> out2)
            in  go s1' s2' queue1' queue2'
