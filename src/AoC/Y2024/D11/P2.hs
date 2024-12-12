module AoC.Y2024.D11.P2 where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text qualified as P
import Data.Bifunctor
import Data.ByteString qualified as BS
import Data.Char qualified as Char
import Data.Foldable
import Data.Functor
import Data.Functor.WithIndex
import Data.IntMap.Strict qualified as IntMap
import Data.List qualified as List
import Data.List.Split qualified as Split
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Traversable
import Linear hiding (E)
import System.FilePath
import Text.Read qualified as Read

import AoC.Common
import AoC.CoordVec qualified as CV

type Loc = CV.Coord

day :: Int
day = 11

----------------------------------------

run :: IO ()
run = do
  strs <- words <$> readReal day
  let row0 = parseStone <$> strs
  l row0
  let shg0 = mkSHG row0
  l shg0
  let shgN = List.iterate' stepSHG shg0
      shg n = head $ drop n shgN
  answer $ sum $ shg 75
  -- let s0 = nextSplit 0
  -- l s0

type Row = [Stone]
type Stone = Int

-- Stone histogram
type SHG = Map.Map Stone Int

mkSHG :: [Stone] -> Map.Map Stone Int
mkSHG = mkHistogram

stepSHG :: SHG -> SHG
stepSHG = Map.unionsWith (+) . map f . Map.toList
 where
  f (stone, count) = (* count) <$> mkSHG (stepStone stone)

-- type SplitMap = IntMap Split
-- data Split = Split
--   { stepCount :: Int
--   , leftStone :: Stone
--   , rightStone :: Stone
--   }
--   deriving (Eq, Ord, Show)

-- nextSplit :: Stone -> (Int, Stone, Stone)
-- nextSplit stone =
--   let (stepCount, [stone1, stone2]) =
--         head . dropWhile ((<= 1) . length . snd) . zip [0..] $ iterateM stepStone [stone]
--         -- Ugh, the Foldable instance for ((,) a) (where `length _ = 1`) screwed me up
--         -- head . dropWhile ((<= 1) . length) . zip [0..] $ iterateM stepStone [stone]
--   in (stepCount, stone1, stone2)

stepStone :: Stone -> [Stone]
stepStone stone
  | stone == 0 = [1]
  | Just (l, r) <- splitEvenDigits stone = [l, r]
  | otherwise = [stone * 2024]

splitEvenDigits :: Int -> Maybe (Int, Int)
splitEvenDigits n =
  let digits = show n
      digitCount = length digits
  in if digitCount `mod` 2 == 0
      then
        Just . bimap read read $ splitAt (digitCount `div` 2) digits
      else
        Nothing

parseStone :: String -> Stone
parseStone = read
