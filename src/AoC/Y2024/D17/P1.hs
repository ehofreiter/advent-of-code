module AoC.Y2024.D17.P1 where

import Control.Applicative ((<|>))
import Control.Lens
import Data.Attoparsec.Text qualified as P
import Data.Bits
import Data.ByteString qualified as BS
import Data.Char qualified as Char
import Data.Either
import Data.Foldable
import Data.Functor
import Data.Functor.WithIndex
import Data.List qualified as List
import Data.List.Split qualified as Split
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Traversable
import Linear hiding (E)
import Safe qualified as Safe
import System.FilePath
import Text.Read qualified as Read

import AoC.Common
import AoC.CoordVec qualified as CV

type Loc = CV.Coord

day, part :: Int
(day, part) = (17, 1)

----------------------------------------

run :: IO ()
run = do
  printHeader day part
  rows <- lines <$> readReal day
  traverse_ l rows
  let [regAStr, regBStr, regCStr, "", progStr] = rows
  let regA0 = read @Integer $ Split.splitOn ": " regAStr !! 1
      regB0 = read @Integer $ Split.splitOn ": " regBStr !! 1
      regC0 = read @Integer $ Split.splitOn ": " regCStr !! 1
      regs0 = Regs regA0 regB0 regC0
      prog = fmap readOpCode . Split.splitOn "," $ Split.splitOn ": " progStr !! 1
      ip0 = 0
  l regA0
  l regB0
  l regC0
  l prog
  -- let loop (ip, regs) outs =
  --       case stepProg prog (ip, regs, outs) of
  --         Nothing -> outs
  --         Just (ip', regs', outs') -> loop (ip', regs') (outs <> outs')
  --     outputs = loop (ip0, regs0) []
  -- l outputs
  let steps = takeWhile Maybe.isJust $ iterateM (stepProg prog) (Just (ip0, regs0, []))
  traverse_ l $ steps
  let Just (ipN, regsN, outsN) = last steps
  let result = List.intercalate "," $ show <$> outsN
  answer result

readOpCode :: String -> OpCode
readOpCode = toEnum . read @Int

data OpCode
  = Adv0
  | Bxl1
  | Bst2
  | Jnz3
  | Bxc4
  | Out5
  | Bdv6
  | Cdv7
  deriving (Bounded, Enum, Eq, Ord, Show)

type Program = [OpCode]

type IP = Int

data Regs = Regs
  { rA :: Integer
  , rB :: Integer
  , rC :: Integer
  }
  deriving (Eq, Ord, Show)

type Outs = [Int]

stepProg :: Program -> (IP, Regs, Outs) -> Maybe (IP, Regs, Outs)
stepProg program (ip, regs, outs) = do
  opCode <- program `Safe.atMay` ip
  literal <- toInteger . fromEnum <$> program `Safe.atMay` (ip+1)
  let combo = case literal of
        0 -> 0
        1 -> 1
        2 -> 2
        3 -> 3
        4 -> rA regs
        5 -> rB regs
        6 -> rC regs
        7 -> error "combo operand 7 invalid"
  case opCode of
    Adv0 -> Just (ip+2, regs { rA = rA regs `div` (2^combo) }, outs)
    Bxl1 -> Just (ip+2, regs { rB = rB regs `xor` literal }, outs)
    Bst2 -> Just (ip+2, regs { rB = combo `mod` 8 }, outs)
    Jnz3 -> Just (if rA regs == 0 then ip+2 else fromIntegral literal, regs, outs)
    Bxc4 -> Just (ip+2, regs { rB = rB regs `xor` rC regs }, outs)
    Out5 -> Just (ip+2, regs, outs <> [fromIntegral (combo `mod` 8)])
    Bdv6 -> Just (ip+2, regs { rB = rA regs `div` (2^combo) }, outs)
    Cdv7 -> Just (ip+2, regs { rC = rA regs `div` (2^combo) }, outs)
