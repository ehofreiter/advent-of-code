module AoC.Y2024.D11.P2 where

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
day = 11

----------------------------------------

run :: IO ()
run = do
  intStrs <- words <$> readExample day
  let ints = read @Int <$> intStrs
  l ints
  -- let gridCV = CV.fromLists rows
  -- l gridCV
  -- let result = _
  -- answer result