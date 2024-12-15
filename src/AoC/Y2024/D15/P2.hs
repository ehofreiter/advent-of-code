{-# LANGUAGE MultiWayIf #-}
module AoC.Y2024.D15.P2 where

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
      room = Map.fromListWith (<>) [(spot, [V2 2 1 * loc]) | (loc, spot) <- CV.toSparseMap sm0]
      wallsL = Set.fromList $ room Map.! Wl
      wallsR = Set.fromList $ boxRFromL <$> room Map.! Wl
      walls = wallsL <> wallsR
      robot = head $ room Map.! Rb
      boxLs = Set.fromList $ room Map.! Bx
      boxRs = Set.fromList $ boxRFromL <$> room Map.! Bx
      size = (2 * CV.rowSize sm0, CV.colSize sm0)

  let rs0 = RS robot boxLs
  let allSteps = allRobotSteps walls moves rs0
  let prs = printRS size walls
  -- for_ allSteps $ \rs ->
  --   prs rs

  -- prs rs0
  let finalRs = last allSteps
  -- prs finalRs
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
showRS (sizeX, sizeY) walls (RS robot boxLs) =
  [[toChar (V2 x y) | x <- [0..sizeX-1]] | y <- [0..sizeY-1]]
 where
  boxRs = Set.map boxRFromL boxLs
  toChar loc
    | Set.member loc walls = '#'
    | Set.member loc boxLs = '['
    | Set.member loc boxRs = ']'
    | loc == robot = '@'
    | otherwise = '.'

boxRFromL :: Loc -> Loc
boxRFromL v = v ^+^ V2 1 0

allRobotSteps :: Walls -> [Dir] -> RS -> [RS]
allRobotSteps walls dirs rs0 =
  List.scanl' (flip (stepRobot walls)) rs0 dirs

stepRobot :: Walls -> Dir -> RS -> RS
stepRobot walls dir rs0 =
  let robot0 = rsRobot rs0
      boxLs0 = rsBoxes rs0
      robot1 = dirToV2 dir ^+^ robot0
      (hitBoxL, otherBoxLs) = Set.partition (robotXbox robot1) boxLs0
  in if
      -- robot blocked by wall, no movement
      | robot1 `Set.member` walls -> rs0
      -- robot free to move, no box movement
      | Set.null hitBoxL -> RS robot1 boxLs0
      -- robot hit box, handle box movement
      | otherwise ->
        case pushBoxes walls dir hitBoxL otherBoxLs of
          -- box blocked by wall, no movement
          Nothing -> rs0
          Just movedBoxLs -> RS robot1 movedBoxLs

robotXbox :: Loc -> Loc -> Bool
robotXbox robot boxL = robot == boxL || robot == boxRFromL boxL

pushBoxes :: Walls -> Dir -> Boxes -> Boxes -> Maybe Boxes
pushBoxes walls dir hitBoxLs' otherBoxLs' = loop hitBoxLs' otherBoxLs'
 where
  dv = dirToV2 dir
  loop hitBoxLs otherBoxLs =
    let movedHitBoxLs = Set.map (dv ^+^) hitBoxLs
        wallHits = Set.filter (wallsXbox walls) movedHitBoxLs
        (newHitBoxLs, newOtherBoxLs) = Set.partition (\loc -> or $ boxXbox loc <$> Set.toList movedHitBoxLs) otherBoxLs
    in if
        -- box hit wall
        | not (Set.null wallHits) -> Nothing
        -- box free to move
        | Set.null newHitBoxLs -> Just $ (movedHitBoxLs <> otherBoxLs)
        -- box hit another box, send help
        | otherwise -> (movedHitBoxLs <>) <$> loop newHitBoxLs newOtherBoxLs

wallsXbox :: Walls -> Loc -> Bool
wallsXbox walls boxL = not $ Set.disjoint walls (Set.fromList [boxL, boxRFromL boxL])

boxXbox :: Loc -> Loc -> Bool
boxXbox boxLA boxLB = or $ (==) <$> [boxLA, boxRFromL boxLA] <*> [boxLB, boxRFromL boxLB]

mkBoxRs :: Set.Set Loc -> Set.Set Loc
mkBoxRs = Set.map boxRFromL

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
