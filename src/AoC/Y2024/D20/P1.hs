module AoC.Y2024.D20.P1 where

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
import Data.Semigroup qualified as Sg
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
(day, part) = (20, 1)

----------------------------------------

run :: IO ()
run = do
  printHeader day part
  rows <- lines <$> readExample day
  traverse_ l rows

  let sm = parseChar <$> CV.fromLists rows
  l sm

  let spotLocs = locsByType sm
  l spotLocs
  printHoriz

  let size = V2 (CV.rowSize sm) (CV.colSize sm)
      walls = Set.fromList $ spotLocs Map.! Wl
      start = getStart spotLocs
      end = getEnd spotLocs
      minCosts0 = Map.singleton start 0
      frontier0 = Map.singleton 0 [start]
  printMap size walls
  printHoriz

  -- let allSteps = iterateM (stepState size walls end) $ Right (minCosts0, frontier0)
  --     steps = rights allSteps
  --     result = head $ lefts $ dropWhile isRight allSteps
  -- answer result

  let loop st0 =
        case stepState size walls end st0 of
          Left c -> c
          Right st1 -> loop st1
      minCostHonest = loop (minCosts0, frontier0)
  putStrLn $ "Cost without cheats: " <> show minCostHonest <> " ps"
  printHoriz

-- Start
data Cheat = Cheat { st :: Loc,  en :: Loc }
  deriving (Eq, Ord, Show)

stepStateCheat :: V2 Int -> Walls -> Loc -> (MinCosts, Frontier, Maybe Cheat) -> Either Cost (MinCosts, Frontier, Maybe Cheat)
stepStateCheat size walls endLoc (minCosts0, frontier0, mCheat0) =
  let (newMinCosts, frontier1, mCheat1) = expandFrontierCheat size walls minCosts0 frontier0 mCheat0
      solutions = Map.filterWithKey (\loc _cost -> loc == endLoc) newMinCosts
  in if Map.null solutions
    then Right (Map.unionWith (<>) minCosts0 newMinCosts, frontier1, mCheat1)
    else Left $ minimum $ Sg.getMin <$> Map.elems solutions

expandFrontierCheat :: V2 Int -> Walls -> MinCosts -> Frontier -> Maybe Cheat -> (MinCosts, Frontier, Maybe Cheat)
expandFrontierCheat size walls minCosts frontier mCheat = case Map.minViewWithKey frontier of
  Nothing -> error "empty frontier"
  Just ((cost, bestNodes), frontier') ->
    let newMinCosts =
          Map.difference
            (Map.unionsWith (<>) $ expandNodeCheat size walls mCheat cost <$> bestNodes)
            minCosts
    in ( newMinCosts
       , Map.unionsWith (<>) $ frontier' : [Map.singleton c [n] | (n, Sg.Min c) <- Map.toList newMinCosts]
       , _
       )

expandNodeCheat :: V2 Int -> Walls -> Maybe Cheat -> Cost -> Loc -> (MinCosts, Maybe Cheat)
expandNodeCheat (V2 sx sy) walls cost loc =
  Map.fromList
    [ (loc', Sg.Min (cost + 1))
      | dir <- [minBound..maxBound]
      , let loc'@(V2 x' y') = loc ^+^ dirToV2 dir
      , Set.notMember loc' walls
      , x' >= 0 && x' < sx
      , y' >= 0 && y' < sy
    ]

type SM = CV.CoordVec (Maybe Spot)

type SpotLocs = Map.Map Spot [Loc]

locsByType :: SM -> SpotLocs
locsByType sm = Map.fromListWith (<>) [(s, [loc]) | (loc, s) <- CV.toSparseMap sm]

getStart :: SpotLocs -> Loc
getStart sl = head $ sl Map.! St

getEnd :: SpotLocs -> Loc
getEnd sl = head $ sl Map.! En

type Cost = Int

type Frontier = Map.Map Cost [Loc]
type MinCosts = Map.Map Loc (Sg.Min Cost)

stepState :: V2 Int -> Walls -> Loc -> (MinCosts, Frontier) -> Either Cost (MinCosts, Frontier)
stepState size walls endLoc (minCosts0, frontier0) =
  let (newMinCosts, frontier1) = expandFrontier size walls minCosts0 frontier0
      solutions = Map.filterWithKey (\loc _cost -> loc == endLoc) newMinCosts
  in if Map.null solutions
    then Right (Map.unionWith (<>) minCosts0 newMinCosts, frontier1)
    else Left $ minimum $ Sg.getMin <$> Map.elems solutions

expandFrontier :: V2 Int -> Walls -> MinCosts -> Frontier -> (MinCosts, Frontier)
expandFrontier size walls minCosts frontier = case Map.minViewWithKey frontier of
  Nothing -> error "empty frontier"
  Just ((cost, bestNodes), frontier') ->
    let newMinCosts =
          Map.difference
            (Map.unionsWith (<>) $ expandNode size walls cost <$> bestNodes)
            minCosts
    in ( newMinCosts
       , Map.unionsWith (<>) $ frontier' : [Map.singleton c [n] | (n, Sg.Min c) <- Map.toList newMinCosts]
       )

expandNode :: V2 Int -> Walls -> Cost -> Loc -> MinCosts
expandNode (V2 sx sy) walls cost loc =
  Map.fromList
    [ (loc', Sg.Min (cost + 1))
      | dir <- [minBound..maxBound]
      , let loc'@(V2 x' y') = loc ^+^ dirToV2 dir
      , Set.notMember loc' walls
      , x' >= 0 && x' < sx
      , y' >= 0 && y' < sy
    ]

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

showSpot :: Walls -> [Loc] -> Loc -> Char
showSpot walls path pos
  | Set.member pos walls = '#'
  | elem pos path = 'O'
  | otherwise = '.'

printMap :: V2 Int -> Walls -> IO ()
printMap size walls = printMapPath size walls []

printMapPath :: V2 Int -> Walls -> [Loc] -> IO ()
printMapPath size walls path = traverse_ putStrLn $ showBestPathLocs size walls path

showBestPathLocs :: V2 Int -> Walls -> [Loc] -> [String]
showBestPathLocs (V2 sizeX sizeY) walls path =
  [[showSpot walls path (V2 x y) | x <- [0..sizeX-1]] | y <- [0..sizeY-1]]

parseChar :: Char -> Maybe Spot
parseChar = \case
  '#' -> Just Wl
  '.' -> Nothing
  'S' -> Just St
  'E' -> Just En
