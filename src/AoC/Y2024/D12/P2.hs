module AoC.Y2024.D12.P2 where

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
  let perims = plotPerimeter pm . snd <$> plots
  -- traverse_ l perims
  let sides = perimToSides <$> perims
  -- traverse_ l sides
  let result = sum [plotArea plotLocs * plotSides pm plotLocs | (plant, plotLocs) <- plots]
  answer result

plotArea :: Set.Set Loc -> Int
plotArea = Set.size

plotSides :: PM -> Set.Set Loc -> Int
plotSides pm plotLocs = Set.size $ perimToSides $ plotPerimeter pm plotLocs

data Dir = Rt | Up | Lf | Dn
  deriving (Eq, Enum, Bounded, Ord, Show)

dirToV2 :: Dir -> V2 Int
dirToV2 = \case
  Rt -> V2 1 0
  Up -> V2 0 (-1)
  Lf -> V2 (-1) 0
  Dn -> V2 0 1

allDirs :: [Dir]
allDirs = [minBound..maxBound]

perimToSides :: Perimeter -> Set.Set (Dir, Set.Set Loc)
perimToSides perim = go perim mempty
 where
  go unseen sides = case Set.lookupMin unseen of
    Nothing -> sides
    Just (loc, dir) ->
      let side = fillPerim (loc, dir) unseen
      in go (unseen Set.\\ Set.map (, dir) (snd side)) (Set.insert side sides)

fillPerim :: (Loc, Dir) -> Perimeter -> (Dir, Set.Set Loc)
fillPerim (loc, dir) perim = (dir, locSet)
 where
  locSet = Set.fromList . toList $ bfs adjs (Seq.singleton loc)
  adjs l = filter (\l' -> Set.member (l', dir) perim) $
    case dir of
      Rt -> (^+^ l) <$> [V2 0 1, V2 0 (-1)]
      Up -> (^+^ l) <$> [V2 1 0, V2 (-1) 0]
      Lf -> (^+^ l) <$> [V2 0 1, V2 0 (-1)]
      Dn -> (^+^ l) <$> [V2 1 0, V2 (-1) 0]

type Perimeter = Set.Set (Loc, Dir)

plotPerimeter :: PM -> Set.Set Loc -> Perimeter
plotPerimeter pm plotLocs = Set.fromList (locPerim =<< Set.toList plotLocs)
 where
  locPerim loc = filter isValid [(loc, dir) | dir <- allDirs]
  isValid (loc, dir) = Set.notMember (loc ^+^ dirToV2 dir) plotLocs

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
