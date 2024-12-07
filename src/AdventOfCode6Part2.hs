module AdventOfCode6Part2 where

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
import Data.Either qualified as Either
import Data.Set qualified as Set
import Data.Text qualified as T
import System.FilePath
import Text.Read qualified as Read
import Linear hiding (E)
import Data.Foldable

import AoC.Common hiding (dataFolder, readData, readExample, readReal, l, answer)
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
-- ghcid -T run2 src/AdventOfCode${day}.hs

run2 :: IO ()
run2 = do
  rows <- lines <$> readReal
  traverse_ l rows
  let spots = fmap spotFromChar <$> rows
  traverse_ l spots
  let sm = CV.fromLists spots
  let guards = allGuards sm
  let walls = possibleWalls sm
  let sms = flip addWall sm <$> Set.toList walls
  let loops = filter doesItLoop sms
  answer (length loops)

addWall :: Pos -> SM -> SM
addWall pos = imap (\p s -> if p == pos then W else s)

possibleWalls :: SM -> Set.Set Pos
possibleWalls sm = Set.fromList . filter (\s -> s /= pos0 && CV.valid sm s) . fmap guardToWall $ allGuards sm
 where
  pos0 = findStartPos sm

guardToWall :: Guard -> Pos
guardToWall g = stepDir (gDir g) (gPos g)

type Step = Either Bool St

allSteps :: SM -> [Step]
allSteps sm =
  let pos0 = findStartPos sm
      mSt0 = Right $ St (Guard dir0 pos0) Set.empty
  in iterateM (stepSt sm) mSt0

allSts :: SM -> [St]
allSts = Either.rights . takeWhile Either.isRight . allSteps

allGuards :: SM -> [Guard]
allGuards = fmap stGuard . allSts

doesItLoop :: SM -> Bool
doesItLoop = head . Either.lefts . allSteps

stepSt :: SM -> St -> Either Bool St
stepSt sm st0@(St guard0@(Guard dir0 pos0) prevs0) =
  case sm CV.!? pos1 of
    Just spot ->
      if Set.member (guard1 spot) prevs0
        then Left True
        else Right $ St (guard1 spot) prevs1
    Nothing ->
      Left False
 where
  pos1 = stepDir dir0 pos0
  guard1 spot = case spot of
    W -> Guard (rot90 dir0) pos0
    _ -> Guard dir0 pos1
  prevs1 = Set.insert guard0 prevs0

findStartPos :: SM -> Pos
findStartPos = head . Maybe.catMaybes . CV.toList . imap (\i s -> if s == S then Just i else Nothing)

findGuard0 :: SM -> Guard
findGuard0 sm = Guard dir0 (findStartPos sm)

data Guard = Guard
  { gDir :: Dir
  , gPos :: Pos
  }
  deriving (Eq, Ord, Show)

data St = St
  { stGuard :: Guard
  , stPrevs :: Set.Set Guard
  }
  deriving (Eq, Ord, Show)

stDir :: St -> Dir
stDir = gDir . stGuard

stPos :: St -> Pos
stPos = gPos . stGuard

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
