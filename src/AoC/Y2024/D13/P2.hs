module AoC.Y2024.D13.P2 where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text qualified as P
import Data.ByteString qualified as BS
import Data.Char qualified as Char
import Data.Either
import Data.Functor
import Data.Functor.WithIndex
import Data.List qualified as List
import Data.List.Split qualified as Split
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text qualified as T
import System.FilePath
import Text.Read qualified as Read
import Linear hiding (E)
import Control.Lens hiding (E)
import Data.Foldable

import AoC.Common
import AoC.CoordVec qualified as CV

type Loc = CV.Coord

day :: Int
day = 13

----------------------------------------

prizeShift :: Int
prizeShift = 10^13

updatePrize :: Machine -> Machine
updatePrize m = m { mPrize = mPrize m ^+^ V2 prizeShift prizeShift }

run :: IO ()
run = do
  rows <- lines <$> readReal day
  let blocks = Split.splitOn [""] rows
      machines = updatePrize . parseMachine <$> blocks
  -- traverse_ l machines
  -- l [5,4..1]
  let solutions = Maybe.mapMaybe solve machines
  traverse_ l solutions
  let costs = moveCost <$> solutions
      result = sum costs
  answer result

type Move = (Int, Int) -- Times button A pressed, times button B pressed

isMoveValid :: Move -> Machine -> Bool
isMoveValid move m = testMove move m == mPrize m

testMove :: Move -> Machine -> V2 Int
testMove (countA, countB) m =
  mMoveA m ^* countA ^+^ mMoveB m ^* countB

moveCost :: Move -> Int
moveCost (countA, countB) = countA * 3 + countB * 1

solve :: Machine -> Maybe Move
solve m =
  let av@(V2 ax ay) = mMoveA m
      bv@(V2 bx by) = mMoveB m
      pv@(V2 px py) = mPrize m
      factors = (div (lcm ax ay) <$> av) * V2 1 (-1)
      (countB, modB) = (mPrize m `dot` factors) `divMod` (mMoveB m `dot` factors)
      (countA, modA) = (px - bx*countB) `divMod` ax
  in if modB == 0 && modA == 0
      then Just (countA, countB)
      else Nothing

machineToEq :: Machine -> Equation
machineToEq m =
  let (V2 ax ay) = mMoveA m
      (V2 bx by) = mMoveB m
      (V2 px py) = mPrize m
  in Equation (V3 ax bx px) (V3 ay by py)

data Equation = Equation
  { eqX :: V3 Int
  , eqY :: V3 Int
  }
  deriving (Eq, Ord, Show)

data Machine = Machine
  { mMoveA :: V2 Int
  , mMoveB :: V2 Int
  , mPrize :: V2 Int
  }
  deriving (Eq, Ord, Show)

parseMachine :: [String] -> Machine
parseMachine strs =
  let [butA, butB, prize] = strs
  in Machine
    { mMoveA = parseXY1 . (!! 1) $ Split.splitOn ": " butA
    , mMoveB = parseXY1 . (!! 1) $ Split.splitOn ": " butB
    , mPrize = parseXY2 . (!! 1) $ Split.splitOn ": " prize
    }

parseXY1 :: String -> V2 Int
parseXY1 str =
  let [xStr, yStr] = Split.splitOn ", " str
  in V2 (parseInt1 xStr) (parseInt1 yStr)

parseXY2 :: String -> V2 Int
parseXY2 str =
  let [xStr, yStr] = Split.splitOn ", " str
  in V2 (parseInt2 xStr) (parseInt2 yStr)

parseInt1 :: String -> Int
parseInt1 = read . (!! 1) . Split.splitOn "+"

parseInt2 :: String -> Int
parseInt2 = read . (!! 1) . Split.splitOn "="
