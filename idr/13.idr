import Data.So

Layer : Type
Layer = (Nat, Nat)

%default total

-- "n: m" -> (n, m)
parseLine : String -> Maybe Layer
parseLine line = 
  case words' $ unpack line of
       [depth, range] =>
           Just (cast . pack . fromMaybe ['0'] . init' $ depth, cast $ pack range)
       _ => Nothing

-- Whether you will be caught by the layer if you wait `delay` picoseconds
-- FIXME: don't use believe_me. Need a proof that LTE 2 range -> Not (rhs = 0)
caught : Nat -> Layer -> Bool
caught delay (depth, range) =
    case 2 `isLTE` range of
        Yes prf => 
            let rhs = 2 * minus range 1
            in  modNatNZ (depth + delay) rhs believe_me == 0
        No  con => True

severity : Layer -> Nat
severity (depth, range) = depth * range

scoreTrip : List Layer -> Nat -> Nat
scoreTrip layers delay = sum . map severity $ filter (caught delay) layers

safeTrip : List Layer -> Nat -> Bool
safeTrip layers delay = all (not . caught delay) layers

part1 : List Layer -> Nat
part1 layers = scoreTrip layers 0

-- FIXME: Big Nats overflow the stack, so we can't go high enough to solve
-- part2. Also this should ideally terminate in a better way than quitting at
-- an arbitrary N.
partial
part2 : List Layer -> Maybe Nat
part2 layers = go (the Nat 1000)
    where go n =
              if safeTrip layers n then
                  Just n
              else if n <= 0 then
                  Nothing
              else
                  go (minus n 1)
partial
main : IO ()
main = do
    file <- readFile "13.txt"
    case file of
         Left  err => putStrLn "File not found"
         Right res => do
             let layers = mapMaybe parseLine $ lines res
             putStrLn $ "Part 1: " <+> show (part1 layers)
             putStrLn $ "Part 2: " <+> fromMaybe "failed" (show <$> part2 layers)
