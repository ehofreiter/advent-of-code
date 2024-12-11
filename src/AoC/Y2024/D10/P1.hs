module AoC.Y2024.D10.P1 where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text qualified as P
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
day = 10

----------------------------------------

run :: IO ()
run = do
  rows <- lines <$> readReal day
  traverse_ l rows
  let hm = parseChar <$> CV.fromLists rows
  l hm
  let ths = trailheads hm
  l ths
  -- let nodes = Map.fromSet (hm CV.!) . trailheadEnds hm <$> ths
  -- l nodes
  let scores = Map.fromSet (trailheadScore hm) (Set.fromList ths)
  l scores
  let result = sum scores
  answer result

trailheadScore :: HM -> Loc -> Int
trailheadScore hm th = Set.size $ trailheadEnds hm th

trailheadEnds :: HM -> Loc -> Set.Set Loc
trailheadEnds hm th =
  let ends = toList $ bfs (getTrailAdj hm) (Seq.singleton th)
  in Set.fromList $ filter (\loc -> hm CV.! loc == 9) ends

getTrailAdj :: HM -> Loc -> [Loc]
getTrailAdj hm loc =
  let height = hm CV.! loc
  in fst <$> filter ((height + 1 ==) . snd) (CV.adjPairs4 hm loc)

type HM = CV.CoordVec Int

trailheads :: HM -> [Loc]
trailheads = CV.elemIndices 0

parseChar :: Char -> Int
parseChar c = read [c]
