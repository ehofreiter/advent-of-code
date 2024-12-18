module AoC.Y2024.D18.P1 where

import Control.Applicative ((<|>))
import Control.Lens
import Data.Attoparsec.Text qualified as P
import Data.Bits
import Data.ByteString qualified as BS
import Data.Char qualified as Char
import Data.Either
import Data.Foldable
import Data.Functor
import Data.Functor.WithIndex
import Data.List qualified as List
import Data.List.Split qualified as Split
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Semigroup qualified as Semigroup
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Traversable
import Linear hiding (E)
import Safe qualified as Safe
import System.FilePath
import Text.Read qualified as Read

import AoC.Common
import AoC.CoordVec qualified as CV

type Loc = CV.Coord

day, part :: Int
(day, part) = (18, 1)

----------------------------------------

loadExample :: IO (Int, [String])
loadExample = do
  rows <- lines <$> readExample day
  pure (6, rows)

loadReal :: IO (Int, [String])
loadReal = do
  rows <- lines <$> readReal day
  pure (70, rows)

run :: IO ()
run = do
  printHeader day part
  (maxCoord, rows) <- loadReal
  let size = maxCoord + 1
  let poses = parsePos <$> rows
  traverse_ l poses
  let start = V2 0 0
      end = V2 (size-1) (size-1)
  let kilobyte = take 1024 $ poses
      walls = Set.fromList kilobyte
  printHoriz
  printMap (size, size) [] walls start
  printHoriz
  let minCosts0 = Map.singleton start 0
      frontier0 = Map.singleton 0 [start]
      allSteps = iterateM (stepState size walls end) $ Right (minCosts0, frontier0)
      steps = rights allSteps
      result = head $ lefts $ dropWhile isRight allSteps
  -- for_ (Safe.headMay $ dropWhile isRight allSteps) $ \case
  --   Left result -> answer result
  --   Right (minCosts, frontier) -> do
  --     putStrLn "frontier:"
  --     traverse_ l $ Map.toList frontier
  --     printHoriz
  answer result
  -- let loop st0 =
  --       case stepState walls endLoc st0 of
  --         Left c -> c
  --         Right st1 -> loop st1
  --     result = loop (minCosts0, frontier0)
  -- let gridCV = CV.fromLists rows
  -- l gridCV
  -- let result = _
  -- answer result

type Cost = Int

type Frontier = Map.Map Cost [Loc]
type MinCosts = Map.Map Loc (Semigroup.Min Cost)

stepState :: Int -> Walls -> Loc -> (MinCosts, Frontier) -> Either Cost (MinCosts, Frontier)
stepState size walls endLoc (minCosts0, frontier0) =
  let (newMinCosts, frontier1) = expandFrontier size walls minCosts0 frontier0
      solutions = Map.filterWithKey (\loc _cost -> loc == endLoc) newMinCosts
  in if Map.null solutions
    then Right (Map.unionWith (<>) minCosts0 newMinCosts, frontier1)
    else Left $ minimum $ Semigroup.getMin <$> Map.elems solutions

expandFrontier :: Int -> Walls -> MinCosts -> Frontier -> (MinCosts, Frontier)
expandFrontier size walls minCosts frontier = case Map.minViewWithKey frontier of
  Nothing -> error "empty frontier"
  Just ((cost, bestNodes), frontier') ->
    let newMinCosts =
          Map.difference
            (Map.unionsWith (<>) $ expandNode size walls cost <$> bestNodes)
            minCosts
    in ( newMinCosts
       , Map.unionsWith (<>) $ frontier' : [Map.singleton c [n] | (n, Semigroup.Min c) <- Map.toList newMinCosts]
       )

expandNode :: Int -> Walls -> Cost -> Loc -> MinCosts
expandNode size walls cost loc =
  Map.fromList
    [ (loc', Semigroup.Min (cost + 1))
      | dir <- [minBound..maxBound]
      , let loc'@(V2 x' y') = loc ^+^ dirToV2 dir
      , Set.notMember loc' walls
      , x' >= 0 && x' < size
      , y' >= 0 && y' < size
    ]

parsePos :: String -> V2 Int
parsePos s = let [x, y] = Split.splitOn "," s in V2 (read x) (read y)

type SM = CV.CoordVec (Maybe Spot)

type SpotLocs = Map.Map Spot [Loc]

data Dir = Rt | Up | Lf | Dn
  deriving (Bounded, Enum, Eq, Ord, Show)

turnR :: Dir -> Dir
turnR = \case
  Rt -> Dn
  Dn -> Lf
  Lf -> Up
  Up -> Rt

turnL :: Dir -> Dir
turnL = \case
  Rt -> Up
  Up -> Lf
  Lf -> Dn
  Dn -> Rt

dirToV2 :: Dir -> V2 Int
dirToV2 = \case
  Rt -> V2 1 0
  Up -> V2 0 (-1)
  Lf -> V2 (-1) 0
  Dn -> V2 0 1

data Spot = Wl | St | En
  deriving (Bounded, Enum, Eq, Ord, Show)

type Walls = Set.Set Loc

showSpot :: [Loc] -> Walls -> Loc -> Loc -> Char
showSpot path walls me pos
  | Set.member pos walls = '#'
  | elem pos path = 'O'
  | me == pos = '@'
  | otherwise = '.'

printMap :: (Int, Int) -> [Loc] -> Walls -> Loc -> IO ()
printMap size path walls me = traverse_ putStrLn $ showBestPathLocs size path walls me

showBestPathLocs :: (Int, Int) -> [Loc] -> Walls -> Loc -> [String]
showBestPathLocs (sizeX, sizeY) path walls me =
  [[showSpot path walls me (V2 x y) | x <- [0..sizeX-1]] | y <- [0..sizeY-1]]
