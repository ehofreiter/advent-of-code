module AoC.Y2024.D16.P2 where

import Control.Lens
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

day, part :: Int
(day, part) = (16, 2)

----------------------------------------

run :: IO ()
run = do
  printHeader day part
  rows <- lines <$> readExample day
  traverse_ l rows
  -- let gridCV = CV.fromLists rows
  -- l gridCV
  -- let result = _
  -- answer result
