module AoC.Y2024.D10.P2 where

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
  let trailSet0 = (: []) <$> ths
  let trailSet9 = head $ drop 9 $ iterate (stepTrails hm) trailSet0
  -- Running this incrementally helped me get the answer when
  -- `findAllTrails` failed me.
  -- let trailSetsN = take 1 $ drop 9 $ iterate (stepTrails hm) trailSet0
  -- for_ trailSetsN $ \trailSet ->
  --   for_ trailSet $ \trail -> do
  --     let end = hm CV.! head trail
  --         start = hm CV.! last trail
  --     l (show trail <> " " <> show end <> " " <> show start)
  let result = Set.size $ Set.fromList trailSet9
  answer result
  -- Later, I fixed `findAllTrails`. Here it is working:
  let result2 = Set.size $ findAllTrails hm ths
  answer result2

type Trail = [Loc]

-- First attempt at writing this function failed. See other comments.
findAllTrails :: HM -> [Loc] -> Set.Set Trail
findAllTrails hm ths = go mempty ((: []) <$> ths)
 where
  go doneTrails trails =
    -- See commented line for my first attempt at writing this function that
    -- failed. Note that the `Num` instance of `V2 Int` meant that `9` was
    -- considered as `V2 9 9`, i.e. the coordinates (9, 9).
    let (done, notDone) = List.partition ((== (9 :: Int)) . (hm CV.!) . head) trails
    -- let (done, notDone) = List.partition ((== 9) . head) trails
        newDone = foldl' (flip Set.insert) doneTrails done
    in case notDone of
        [] ->
          newDone
        _ ->
          let newTrails = stepTrails hm notDone
          in go newDone newTrails

stepTrails :: HM -> [Trail] -> [Trail]
stepTrails hm trails =
  stepTrail hm =<< trails

stepTrail :: HM -> Trail -> [Trail]
stepTrail hm trail =
  let nexts = getTrailAdj hm (head trail)
  in [n : trail | n <- nexts]

getTrailAdj :: HM -> Loc -> [Loc]
getTrailAdj hm loc =
  let height = hm CV.! loc
  in fst <$> filter ((height + 1 ==) . snd) (CV.adjPairs4 hm loc)

type HM = CV.CoordVec Int

trailheads :: HM -> [Loc]
trailheads = CV.elemIndices 0

parseChar :: Char -> Int
parseChar c = read [c]
