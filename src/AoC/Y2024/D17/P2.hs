module AoC.Y2024.D17.P2 where

import Control.Lens
import Control.Applicative ((<|>))
import Data.Attoparsec.Text qualified as P
import Data.Bits
import Data.ByteString qualified as BS
import Data.Char qualified as Char
import Data.Either
import Data.Functor
import Data.Functor.WithIndex
import Safe qualified as Safe
import Data.List qualified as List
import Data.List.Split qualified as Split
import Data.Map.Strict qualified as Map
import Data.IntMap.Strict qualified as IntMap
import Data.Maybe qualified as Maybe
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Traversable
import System.FilePath
import Text.Read qualified as Read
import Linear hiding (E)
import Data.Foldable

import AoC.Common
import AoC.CoordVec qualified as CV

type Loc = CV.Coord

day, part :: Int
(day, part) = (17, 2)

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

  printHoriz
  let targets = fromEnum <$> prog
  -- let loop stepTargets bms =
  --       case stepTargets of
  --         [] ->
  --           bms
  --         (step, target) : stepTargets' ->
  --           let bms' = stepTarget bms (step, target)
  --           in loop stepTargets' bms'
  let finalBitMaps = foldl' stepTarget [IntMap.empty] (zip [0..] targets)
      scores = minInt <$> finalBitMaps
  -- traverse_ l scores
  let result = minimum scores
  answer result

minInt :: BitMap -> Integer
minInt bm = sum [2^i | (i, b) <- IntMap.toList bm, b]

-- Set of constraints on what the integer can be.
type BitMap = IntMap.IntMap Bool

printBitMap :: BitMap -> IO ()
printBitMap = putStrLn . showBitMap

-- Serializes a `BitMap` as "BM" followed by the string of known bits. Uses `.`
-- to represent unknown bits to the right of the most significant known bit.
showBitMap :: BitMap -> String
showBitMap bm =
  case IntMap.lookupMax bm of
    Nothing -> "BM ."
    Just (maxI, _) -> "BM " <> [toChar (IntMap.lookup i bm) | i <- reverse [0..maxI]]
 where
  toChar = \case
    Nothing -> '.'
    Just True -> '1'
    Just False -> '0'

stepTarget :: [BitMap] -> (Int, Int) -> [BitMap]
stepTarget bms (step, target) =
  genConstraints =<< [0..7 :: Int]
 where
  genConstraints :: Int -> [BitMap]
  genConstraints a = Maybe.catMaybes $ do
    let shiftAmount = a .^. 3
        xorMask = shiftAmount .^. 5 -- == a .^. 3 .^. 5 == a .^. 6
    bm <- bms
    pure $ catCons
      [ (mkConstraints (step*3) a)
      , (mkConstraints (step*3 + shiftAmount) (xorMask .^. target))
      , bm
      ]

catCons :: [BitMap] -> Maybe BitMap
catCons = \case
  [] -> Nothing
  x:xs ->
    let f mbmX bmY = do
          bmX <- mbmX
          combineConstraints bmX bmY
    in foldl' f (Just x) xs

combineConstraints :: BitMap -> BitMap -> Maybe BitMap
combineConstraints x y =
  let xAndY = IntMap.intersectionWith (<>) (Set.singleton <$> x) (Set.singleton <$> y)
  in if any ((> 1) . Set.size) xAndY
      then Nothing
      else Just $ IntMap.union x y

mkConstraints :: Int -> Int -> BitMap
mkConstraints shift threeBit =
  IntMap.fromList [(i + shift, testBit threeBit i) | i <- [0,1,2]]

programLoop :: (Integer, [Integer]) -> Maybe (Integer, [Integer])
programLoop (a0, outs) =
  let ax3 = (a0 `mod` 8) .^. 3
      c = a0 .>>. fromIntegral ax3
      ax6 = ax3 .^. 5
      b = (ax6 .^. c) `mod` 8
  in if a0 == 0
      then Nothing
      else Just (a0 .>>. 3, outs <> [b])

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
