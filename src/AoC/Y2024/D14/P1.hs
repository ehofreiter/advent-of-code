module AoC.Y2024.D14.P1 where

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
day = 14

----------------------------------------

type RoomSize = V2 Int

loadExample :: IO ([PosVel], RoomSize)
loadExample = do
  rows <- lines <$> readExample day
  pure (parseLine <$> rows, V2 11 7)

loadReal :: IO ([PosVel], RoomSize)
loadReal = do
  rows <- lines <$> readReal day
  pure (parseLine <$> rows, V2 101 103)

run :: IO ()
run = do
  (posVels, roomSize) <- loadReal
  traverse_ l posVels
  l roomSize
  let finalPoses =
        posVels <&> \(PosVel pos vel) ->
          liftI2 mod (pos ^+^ (vel ^* 100)) roomSize
  l finalPoses
  let quadPoses = Maybe.mapMaybe (\p -> flip Map.singleton [p] <$> (quadrant roomSize p)) finalPoses
  l quadPoses
  let quadCounts = length <$> Map.unionsWith (<>) quadPoses
  l quadCounts
  let result = product . Map.elems $ quadCounts
  answer result

data Quadrant = Q1 | Q2 | Q3 | Q4
  deriving (Bounded, Enum, Eq, Ord, Show)

quadrant :: RoomSize -> V2 Int -> Maybe Quadrant
quadrant roomSize@(V2 rsx rsy) (V2 x y)
  | x < hsx && y < hsy = Just Q1
  | x > hsx && y < hsy = Just Q2
  | x < hsx && y > hsy = Just Q3
  | x > hsx && y > hsy = Just Q4
  | otherwise = Nothing
 where
  halfRoomSize@(V2 hsx hsy) = liftI2 div (roomSize - V2 1 1) 2

data PosVel = PosVel { pos :: V2 Int, vel :: V2 Int }
  deriving (Eq, Ord, Show)

parseLine :: String -> PosVel
parseLine str =
  let [[px,py],[vx,vy]] = fmap (Split.splitOn "," . (!! 1) . Split.splitOn "=") $ words str
  in PosVel (V2 (read px) (read py)) (V2 (read vx) (read vy))
