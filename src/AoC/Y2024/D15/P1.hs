module AoC.Y2024.D15.P1 where

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

day :: Int
day = 15

----------------------------------------

run :: IO ()
run = do
  rows <- lines <$> readReal day
  [roomRows, moveRows] <- pure $ Split.splitOn [""] rows
  let moves = parseDir <$> concat moveRows
  l moves
  -- traverse_ l rows
  let sm0 = parseSpot <$> CV.fromLists roomRows
      room = Map.fromListWith (<>) [(spot, [loc]) | (loc, spot) <- CV.toSparseMap sm0]
      walls = Set.fromList $ room Map.! Wl
      robot = head $ room Map.! Rb
      boxes = Set.fromList $ room Map.! Bx
      size = (CV.rowSize sm0, CV.colSize sm0)
  l room
  l "walls"
  l walls
  l "boxes"
  l boxes
  l "robot"
  l robot
  let rs0 = RS robot boxes
  let allSteps = allRobotSteps walls moves rs0
  let prs = printRS size walls
  -- for_ allSteps $ \rs ->
  --   prs rs
  let finalRs = last allSteps
  prs finalRs
  let boxGps = gps <$> Set.toList (rsBoxes finalRs)
  let result = sum boxGps
  answer result
  -- let result = _
  -- answer result

gps :: Loc -> Int
gps (V2 x y) = 100*y + x

printRS :: (Int, Int) -> Walls -> RS -> IO ()
printRS size walls rs = traverse_ putStrLn $ showRS size walls rs

showRS :: (Int, Int) -> Walls -> RS -> [String]
showRS (sizeX, sizeY) walls (RS robot boxes) =
  [[toChar (V2 x y) | x <- [0..sizeX-1]] | y <- [0..sizeY-1]]
 where
  toChar loc
    | Set.member loc walls = '#'
    | Set.member loc boxes = 'O'
    | loc == robot = '@'
    | otherwise = '.'

allRobotSteps :: Walls -> [Dir] -> RS -> [RS]
allRobotSteps walls dirs rs0 =
  List.scanl' (flip (stepRobot walls)) rs0 dirs

stepRobot :: Walls -> Dir -> RS -> RS
stepRobot walls dir rs0 =
  let robot0 = rsRobot rs0
      boxes0 = rsBoxes rs0
      robot1 = dirToV2 dir ^+^ robot0
  in if robot1 `Set.member` walls
      -- robot itself blocked by wall
      then rs0
      else case findEnd dir robot1 boxes0 of
        -- empty space for robot to go
        Nothing -> rs0 { rsRobot = robot1 }
        -- trying to push last box in line to endLoc
        Just endLoc
          -- box line is blocked, so no change
          | endLoc `Set.member` walls -> rs0
          | otherwise ->
              RS
                { rsRobot = robot1
                , rsBoxes = Set.insert endLoc $ Set.delete robot1 boxes0
                }

findEnd :: Dir -> Loc -> Boxes -> Maybe Loc
findEnd dir endLoc boxes =
  if Set.notMember endLoc boxes
    then Nothing
    else Just $ loop (dv ^+^ endLoc)
 where
  dv = dirToV2 dir
  loop el =
    if Set.notMember el boxes
      then el
      else loop (el ^+^ dv)

dirToV2 :: Dir -> V2 Int
dirToV2 = \case
  Rt -> V2 1 0
  Up -> V2 0 (-1)
  Lf -> V2 (-1) 0
  Dn -> V2 0 1

type Boxes = Set.Set Loc
type Walls = Set.Set Loc

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

type Room = Map.Map Loc Spot

data RS = RS
  { rsRobot :: Loc
  , rsBoxes :: Boxes
  }
  deriving (Eq, Ord, Show)

data Dir = Rt | Up | Lf | Dn
  deriving (Bounded, Enum, Eq, Ord, Show)

parseDir :: Char -> Dir
parseDir = \case
  '>' -> Rt
  '^' -> Up
  '<' -> Lf
  'v' -> Dn

type SM = CV.CoordVec Spot

data Spot = Wl | Bx | Rb
  deriving (Bounded, Enum, Eq, Ord, Show)

parseSpot :: Char -> Maybe Spot
parseSpot = \case
  '#' -> Just Wl
  'O' -> Just Bx
  '.' -> Nothing
  '@' -> Just Rb
