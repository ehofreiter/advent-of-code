module AoC.Y2024.D08.P1 where

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
import Data.Set qualified as Set
import Data.Text qualified as T
import System.FilePath
import Text.Read qualified as Read
import Linear hiding (E)
import Data.Foldable

import AoC.Common
import AoC.CoordVec qualified as CV

day :: Int
day = 8

----------------------------------------

run :: IO ()
run = do
  rows <- lines <$> readReal day
  traverse_ l rows
  let gridCV = CV.fromLists rows
  -- l gridCV
  let antLocs = CV.toSparseMap $ charToAnt <$> gridCV
  -- let antLocs = Maybe.catMaybes $ CV.toList $ antLocsCV
  l antLocs
  -- let antLocsMap = Map.fromListWith (<>) $ antLocs
  -- l antLocsMap
  -- let size = length rows
  -- l size
  -- let antiNodes = Set.unions $ genAntiNodes size <$> Map.elems antLocsMap
  -- l antiNodes
  -- let result = Set.size antiNodes
  -- answer result

type Loc = CV.Coord

genAntiNodes :: Int -> [Loc] -> Set.Set Loc
genAntiNodes size locs =
  let antLocPairs = choose2 locs
  in Set.unions $ genForPair size <$> antLocPairs

genForPair :: Int -> (Loc, Loc) -> Set.Set Loc
genForPair size (x, y) =
  let d = x - y
  in Set.fromList $ filter (inGrid size) [x + d, y - d]

inGrid :: Int -> Loc -> Bool
inGrid size (V2 x y) = x >= 0 && x < size && y >= 0 && y < size

charToAnt :: Char -> Maybe Char
charToAnt = \case
  '.' -> Nothing
  c -> Just c
