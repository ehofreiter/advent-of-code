module AoC.Y2024.D09.P2 where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text qualified as P
import Data.ByteString qualified as BS
import Data.Char qualified as Char
import Data.Functor
import Data.Function
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

type Loc = Int

day :: Int
day = 9

----------------------------------------

run :: IO ()
run = do
  digits <- map (read . (:[])) . head . lines <$> readReal day :: IO [Int]
  l digits
  let (fileMap, freeBlocks) = diskLayout digits
  l fileMap
  l freeBlocks
  let files = formatted fileMap (Map.fromList freeBlocks)
  l files
  let result = sum $ map checkFile $ Map.toList files
  answer result

checkFile :: (FileId, Block) -> Int
checkFile (fid, (loc, sz)) = sum $ zipWith (*) [loc..] $ replicate sz fid

type Block = (Loc, Size)

-- gives free blocks in reverse order
diskLayout :: [Size] -> (Map.Map FileId Block, [Block])
diskLayout = go (Map.empty, []) 0 0 True
 where
  go (resultFiles, resultFrees) fileId loc isData = \case
    [] -> (resultFiles, resultFrees)
    size:sizes ->
      if isData
        then go (Map.insert fileId (loc, size) resultFiles, resultFrees) (fileId + 1) (loc + size) (not isData) sizes
        else go (resultFiles, (loc, size):resultFrees) fileId (loc + size) (not isData) sizes

type FileId = Int
type Size = Int

formatted :: Map.Map FileId Block -> Map.Map Loc Size -> Map.Map FileId Block
formatted fileMap freeMap =
  case Map.maxViewWithKey fileMap of
    Nothing -> Map.empty
    Just ((fileId, (fileLoc, fileSize)), fileMap') ->
      let candidateFreeBlocks = Map.filterWithKey (\freeLoc freeSize -> freeLoc < fileLoc && freeSize >= fileSize) freeMap
      in case Map.lookupMin candidateFreeBlocks of
        Nothing ->
          Map.insert fileId (fileLoc, fileSize) $ formatted fileMap' freeMap
        Just (freeLoc, freeSize) ->
          let addFile = Map.insert fileId (freeLoc, fileSize)
              freeSize' = freeSize - fileSize
              freeLoc' = freeLoc + fileSize
              freeMap' = Map.delete freeLoc freeMap
              freeMap'' = if freeSize' == 0 then freeMap' else Map.insert freeLoc' freeSize' freeMap'
          in addFile $ formatted (addFile fileMap') freeMap''
