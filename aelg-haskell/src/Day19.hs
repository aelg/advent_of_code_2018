module Day19 ( solve ) where

import           Control.Arrow
import           Control.Monad
import           Data.List
import           Data.Monoid
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Maybe
import           Data.Char
import           Data.Bits
import           Text.ParserCombinators.ReadP
import qualified Parsing as P
import qualified Utils as U
import Debug.Trace

data Instruction = Instruction Int Int Int Int deriving Show

type Registers = [Int]

data Test = Test Registers Registers Instruction deriving Show

irchoose :: ReadP Int
irchoose = do
  string "#ip "
  P.integer

parser = do
  program <- many parseInstruction
  eof
  return program
  where 
    parseInstruction = do
      skipSpaces
      code <- (addr <++ addi <++mulr <++ muli <++ banr <++ bani <++ borr <++ bori <++ setr <++ seti <++ gtir <++ gtri <++ gtrr <++ eqir <++ eqri <++eqrr)
      a <- P.integerAnd (char ' ')
      b <- P.integerAnd (char ' ')
      c <- P.integerAnd (char '\n')
      return $ Instruction code a b c

test = [ "#ip 0"
       , "seti 5 0 1"
       , "seti 6 0 2"
       , "addi 0 1 0"
       , "addr 1 2 3"
       , "setr 1 0 0"
       , "seti 8 0 4"
       , "seti 9 0 5"
       ]

parse test = (P.run irchoose . head $ test, P.run parser . unlines . drop 1 $ test)

updateReg f n xs = (take (n) $! xs) ++ (f $! v) : (drop (n+1) $! xs)
  where
    v = xs !! n


setReg v = updateReg (const v)

getReg r i = i !! r

rr op (Instruction _ a b _) reg = op (getReg a reg) (getReg b reg)
ri op (Instruction _ a b _) reg = op (getReg a reg) b
ir op (Instruction _ a b _) reg = op a (getReg b reg)

toInt op a b
  | op a b = 1
  | otherwise = 0

runOp op instr@(Instruction _ _ _ c) reg = setReg (op instr reg) c reg

addr = string "addr " >> return 0
addi = string "addi " >> return 1
mulr = string "mulr " >> return 2
muli = string "muli " >> return 3
banr = string "banr " >> return 4
bani = string "bani " >> return 5
borr = string "borr " >> return 6
bori = string "bori " >> return 7
setr = string "setr " >> return 8
seti = string "seti " >> return 9
gtir = string "gtir " >> return 10
gtri = string "gtri " >> return 11
gtrr = string "gtrr " >> return 12
eqir = string "eqir " >> return 13
eqri = string "eqri " >> return 14
eqrr = string "eqrr " >> return 15

operations =
  [ rr (+)
  , ri (+)
  , rr (*)
  , ri (*)
  , rr (.&.)
  , ri (.&.)
  , rr (.|.)
  , ri (.|.)
  , rr const
  , ir const
  , rr (toInt (>))
  , ri (toInt (>))
  , ir (toInt (>))
  , rr (toInt (==))
  , ri (toInt (==))
  , ir (toInt (==))
  ]

runStep reg instr@(Instruction opCode _ _ _) = runOp (operations !! opCode) instr $! reg

loadInstruction ir regs program
  | ip < 0 = Nothing
  | otherwise = listToMaybe $ drop ip program
  where
    ip = (regs !! ir)

runProgram ir program (_,regs)
  | isJust $! instr = (fromJust instr, updateReg (+1) ir $! runStep regs $! (fromJust $! instr))
  | otherwise = (Instruction 0 0 0 0, regs)
  where
    instr = loadInstruction ir regs program

--solve1 :: [String] -> String
solve1 (ir, program) = unlines . map show . take 10000000 $ iterate' ((runProgram $! ir) $! program) (Instruction 0 0 0 0, [0,0,0,0,0,0])

--solve2 :: [String] -> String
solve2 = solve1

solve :: [String] -> (String, String)
solve = parse ^>> solve1 &&& solve2
