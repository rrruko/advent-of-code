{-# LANGUAGE OverloadedStrings #-}

module Day18 where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

data Computer = Computer {
    _ptr :: Int,
    _registers :: Vector Int,
    _sound :: Int,
    _recover :: Int
} deriving Show

initState :: Computer
initState = Computer {
    _ptr = 0,
    _registers = V.replicate 26 0,
    _sound = 0,
    _recover = 0
}

type Reg = Char

data Arg = R Reg | L Int 
    deriving Show

data Instruction =
    Snd Int |
    Set Reg Arg |
    Add Reg Arg |
    Mul Reg Arg |
    Mod Reg Arg |
    Rcv Int     |
    Jgz Arg Arg
    deriving Show

type Parser = Parsec Void String

parseInstruction :: Parser Instruction
parseInstruction = choice
    [ Snd <$ "snd " <*> int
    , Set <$ "set " <*> reg <*> (space *> arg)
    , Add <$ "add " <*> reg <*> (space *> arg)
    , Mul <$ "mul " <*> reg <*> (space *> arg)
    , Mod <$ "mod " <*> reg <*> (space *> arg)
    , Rcv <$ "rcv " <*> int
    , Jgz <$ "jgz " <*> arg <*> (space *> arg)
    ]

parseInstructions :: Parser [Instruction]
parseInstructions = some (parseInstruction <* space)

int :: Parser Int
int = decimal

reg :: Parser Reg
reg = oneOf ['a'..'z']

arg :: Parser Arg
arg = R <$> reg <|> L <$> int

{-
step :: Computer -> Instruction -> Computer
step c@(Computer ptr reg snd rcv) ins = 
    case ins of
        Snd new -> Computer (ptr+1) reg           new rcv
        Set t s -> Computer (ptr+1) (set reg t s) snd rcv
        Add t s -> Computer (ptr+1) (add reg t s) snd rcv
        Mul t s -> Computer (ptr+1) (mul reg t s) snd rcv
        Mod t s -> Computer (ptr+1) (mod reg t s) snd rcv
        Rcv n   -> Computer (ptr+1) reg           snd (if n == 0 then rcv else snd)
        Jgz t s -> Computer (jgz c t s) reg       snd rcv
-}

part1 :: [Instruction] -> Int
part1 = undefined

main :: IO ()
main = do
    file <- readFile "18.txt"
    case parseMaybe parseInstructions file of
        Nothing -> putStrLn "Parse failed."
        Just instructions -> do
            putStr $ "Part 1: "
            print $ part1 instructions
