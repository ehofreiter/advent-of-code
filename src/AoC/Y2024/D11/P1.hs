module AoC.Y2024.D11.P1 where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text qualified as P
import Data.Bifunctor
import Data.ByteString qualified as BS
import Data.Char qualified as Char
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
day = 11

----------------------------------------

run :: IO ()
run = do
  strs <- words <$> readReal day
  let row0 = parseStone <$> strs
  l row0
  let steps = iterateM stepStone row0
      step n = head $ drop n steps
  l $ step 0
  l $ step 1
  l $ step 6
  let result = length $ step 25
  answer result

type Row = [Stone]
type Stone = Int

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
