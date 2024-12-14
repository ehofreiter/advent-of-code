module AoC.Y2024.D13.P1 where

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
import Data.Foldable

import AoC.Common
import AoC.CoordVec qualified as CV

type Loc = CV.Coord

day :: Int
day = 13

----------------------------------------

run :: IO ()
run = do
  rows <- lines <$> readReal day
  let blocks = Split.splitOn [""] rows
      machines = parseMachine <$> blocks
  traverse_ l machines
  -- l [5,4..1]
  let costs = machineCost <$> machines
      result = sum $ Maybe.fromMaybe 0 <$> costs
  answer result

type Move = (Int, Int) -- Times button A pressed, times button B pressed

machineCost :: Machine -> Maybe Int
machineCost m = moveCost <$> Maybe.listToMaybe (validMoves m)

validMoves :: Machine -> [Move]
validMoves m = filter (flip isMoveValid m) (allMoves m)

allMoves :: Machine -> [Move]
allMoves m =
  let V2 maxAX maxAY = liftI2 div (mPrize m) (mMoveA m)
      V2 maxBX maxBY = liftI2 div (mPrize m) (mMoveB m)
      maxA = min maxAX maxAY
      maxB = min maxBX maxBY
  in (,) <$> [maxA, maxA-1 .. 0] <*> [maxB, maxB-1 .. 0]

isMoveValid :: Move -> Machine -> Bool
isMoveValid move m = testMove move m == mPrize m

testMove :: Move -> Machine -> V2 Int
testMove (countA, countB) m =
  mMoveA m ^* countA ^+^ mMoveB m ^* countB

moveCost :: Move -> Int
moveCost (countA, countB) = countA * 3 + countB * 1

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
