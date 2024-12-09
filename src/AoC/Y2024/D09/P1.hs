module AoC.Y2024.D09.P1 where

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

type Loc = CV.Coord

day :: Int
day = 9

----------------------------------------

run :: IO ()
run = do
  digits <- map (read . (:[])) . head . lines <$> readReal day :: IO [Int]
  -- l digits
  let blocks0 = diskLayout digits
  -- l blocks0
  let (dataBlocks, freeBlocks) = List.partition isData blocks0
  -- l datas0
  let fileMap = Map.fromList $ zip [0..] $ Maybe.mapMaybe unD dataBlocks
  l fileMap
  l (diskUsage fileMap)
  let frees = Maybe.mapMaybe unF freeBlocks
  l freeBlocks
  let formattedDisk = formatted fileMap frees
  l formattedDisk
  let result = sum $ zipWith (*) [0..] formattedDisk
  answer result

data Block = D Size | F Size
  deriving (Eq, Ord, Show)

unD :: Block -> Maybe Int
unD = \case
  D i -> Just i
  _ -> Nothing

unF :: Block -> Maybe Int
unF = \case
  F i -> Just i
  _ -> Nothing

isData :: Block -> Bool
isData = \case
  D _ -> True
  _ -> False

diskLayout :: [Size] -> [Block]
diskLayout = zipWith ($) (cycle [D, F])

type FileId = Int
type Size = Int

diskUsage :: Map.Map FileId Size -> Size
diskUsage = sum

formatted :: Map.Map FileId Size -> [Size] -> [FileId]
formatted = grabData
 where
  grabData fileMap frees =
    case Map.minViewWithKey fileMap of
      Just ((fid, sz), fileMap') -> replicate sz fid <> grabFree fileMap' frees
      Nothing -> []
  grabFree fileMap frees =
    case (frees, Map.maxViewWithKey fileMap) of
      (free:frees', Just ((fid, sz), fileMap'))
        | free > sz -> replicate sz fid <> grabFree fileMap' ((free - sz):frees')
        | free == sz -> replicate sz fid <> grabData fileMap' frees'
        | free < sz -> replicate free fid <> grabData (Map.insert fid (sz - free) fileMap') frees'
      _ -> []
