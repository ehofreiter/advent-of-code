{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module AdventOfCode6 where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text qualified as P
import Data.ByteString qualified as BS
import Data.Char qualified as Char
import Data.Functor (($>))
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

import AoC.Common hiding (l, dataFolder, readData, readExample, readReal, answer)
import AoC.CoordVec qualified as CV

day :: Int
day = 6

dataFolder :: FilePath
dataFolder = "data/input/day" <> show day

readData :: FilePath -> IO String
readData file = readFile $ dataFolder </> file

readExample :: IO String
readExample = readData "example.txt"

readReal :: IO String
readReal = readData "real.txt"

l :: (Show a) => a -> IO ()
--l _x = pure ()
l = print

answer :: (Show a) => a -> IO ()
answer x = do
  putStrLn $ "ANSWER:"
  print x

----------------------------------------
-- ghcid -T run1 src/AdventOfCode${day}.hs

run1 :: IO ()
run1 = do
  rows <- lines <$> readReal
  traverse_ l rows
  let spots = fmap spotFromChar <$> rows
  traverse_ l spots
  let spotsCV = CV.fromLists spots
  -- l spotsCV
  l "----"
  let pos0 = findStartPos spotsCV
  l pos0
  let st0 = St dir0 pos0
  l st0
  l "----"
  let allSteps = takeWhile Maybe.isJust $ iterateM (stepSt spotsCV) (Just st0)
  -- traverse_ l allSteps
  let result = Set.size $ Set.fromList $ fmap stPos $ Maybe.catMaybes allSteps
  answer result

stepSt :: SM -> St -> Maybe St
stepSt sm (St dir0 pos0) =
  case sm CV.!? pos1 of
    Just W -> Just $ St (rot90 dir0) pos0
    Just _ -> Just $ St dir0 pos1
    Nothing -> Nothing
 where
  pos1 = stepDir dir0 pos0

findStartPos :: SM -> Pos
findStartPos = head . Maybe.catMaybes . CV.toList . imap (\i s -> if s == S then Just i else Nothing)

data St = St
  { stDir :: Dir
  , stPos :: Pos
  }
  deriving (Eq, Ord, Show)

stepDir :: Dir -> Pos -> Pos
stepDir d pos = dirV d + pos

dir0 :: Dir
dir0 = Up

data Dir = Up | Rt | Dn | Lf
  deriving (Eq, Ord, Show, Enum, Bounded)

dirV :: Dir -> V2 Int
dirV = \case
  Up -> V2 0 (-1)
  Rt -> V2 1 0
  Dn -> V2 0 1
  Lf -> V2 (-1) 0

rot90 :: Dir -> Dir
rot90 = \case
  Up -> Rt
  Rt -> Dn
  Dn -> Lf
  Lf -> Up

type SM = CV.CoordVec Spot
type Pos = CV.Coord

data Spot = E | W | S
  deriving (Eq, Ord, Show, Enum, Bounded)

spotFromChar :: Char -> Spot
spotFromChar = \case
  '.' -> E
  '#' -> W
  '^' -> S

spotToChar :: Spot -> Char
spotToChar = \case
  E -> '.'
  W -> '#'
  S -> '^'

----------------------------------------
-- ghcid -T run2 src/AdventOfCode${day}.hs

run2 :: IO ()
run2 = do
  rows <- lines <$> readExample
  l rows
  -- let result = sum middlePages
  -- answer result

