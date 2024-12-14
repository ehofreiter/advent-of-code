module AoC.Y2024.D12.P1 where

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
day = 12

----------------------------------------

run :: IO ()
run = do
  rows <- lines <$> readReal day
  traverse_ l rows
  let pm = CV.fromLists rows
  let allLocs = Set.fromList $ CV.allCoords pm
  -- l allLocs
  let steps = iterateM (stepFill pm) (Right (allLocs, []))
  Left plots <- pure . head $ dropWhile isRight steps
  let result = sum [plotArea plotLocs * plotPerimeter pm plotLocs | (plant, plotLocs) <- plots]
  answer result

plotArea :: Set.Set Loc -> Int
plotArea = Set.size

plotPerimeter :: PM -> Set.Set Loc -> Int
plotPerimeter pm plotLocs = sum $ locPerim <$> Set.toList plotLocs
 where
  locPerim loc = 4 - Set.size (Set.fromList (CV.adjCoords4 pm loc) `Set.intersection` plotLocs)

-- Plant map
type PM = CV.CoordVec Char
type Plots = [Plot]
type Plot = (Char, Set.Set Loc)

stepFill :: PM -> (Set.Set Loc, Plots) -> Either Plots (Set.Set Loc, Plots)
stepFill pm (unseen, plots) =
  case Set.lookupMin unseen of
    Nothing ->
      Left plots
    Just loc0 ->
      let newPlot = fill loc0 pm
      in Right (unseen Set.\\ snd newPlot, newPlot : plots)

fill :: Loc -> PM -> Plot
fill startLoc pm = (plant, locSet)
 where
  plant = pm CV.! startLoc
  locSet = Set.fromList . toList $ bfs adjs (Seq.singleton startLoc)
  adjs = fmap fst . filter ((== plant) . snd) . CV.adjPairs4 pm
