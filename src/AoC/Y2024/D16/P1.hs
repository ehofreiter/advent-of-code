module AoC.Y2024.D16.P1 where

import Control.Lens
import Control.Applicative ((<|>))
import Data.Attoparsec.Text qualified as P
import Data.ByteString qualified as BS
import Data.Char qualified as Char
import Data.Either
import Data.Functor
import Data.Functor.WithIndex
import Data.Semigroup qualified as Semigroup
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

day, part :: Int
(day, part) = (16, 1)

----------------------------------------

run :: IO ()
run = do
  printHeader day part
  rows <- lines <$> readReal day
  traverse_ l rows
  let spotLocs = locsByType $ parseChar <$> CV.fromLists rows
  l spotLocs
  let walls = Set.fromList $ spotLocs Map.! Wl
      endLoc = getEnd spotLocs
      minCosts0 = initMinCosts spotLocs
      frontier0 = initFrontier spotLocs
  --     allSteps = iterateM (stepState walls endLoc) $ Right (minCosts0, frontier0)
  -- printHoriz
  -- for_ (take 1 $ dropWhile isRight allSteps) $ \case
  --   Left result -> answer result
  --   Right (minCosts, frontier) -> do
  --     putStrLn "frontier:"
  --     traverse_ l $ Map.toList frontier
  --     printHoriz
  let loop st0 =
        case stepState walls endLoc st0 of
          Left c -> c
          Right st1 -> loop st1
      result = loop (minCosts0, frontier0)
  printHoriz
  answer result

type SM = CV.CoordVec (Maybe Spot)

type SpotLocs = Map.Map Spot [Loc]

locsByType :: SM -> SpotLocs
locsByType sm = Map.fromListWith (<>) [(s, [loc]) | (loc, s) <- CV.toSparseMap sm]

getStart :: SpotLocs -> Loc
getStart sl = head $ sl Map.! St

getEnd :: SpotLocs -> Loc
getEnd sl = head $ sl Map.! En

type Cost = Int

type Frontier = Map.Map Cost [(Loc, Dir)]
type MinCosts = Map.Map (Loc, Dir) (Semigroup.Min Cost)

initFrontier :: SpotLocs -> Frontier
initFrontier sl = Map.singleton 0 [(getStart sl, Rt)]

initMinCosts :: SpotLocs -> MinCosts
initMinCosts sl = Map.singleton (getStart sl, Rt) 0

stepState :: Walls -> Loc -> (MinCosts, Frontier) -> Either Cost (MinCosts, Frontier)
stepState walls endLoc (minCosts0, frontier0) =
  let (newMinCosts, frontier1) = expandFrontier walls minCosts0 frontier0
      solutions = Map.filterWithKey (\(loc, _dir) _cost -> loc == endLoc) newMinCosts
  in if Map.null solutions
    then Right (Map.unionWith (<>) minCosts0 newMinCosts, frontier1)
    else Left $ minimum $ Semigroup.getMin <$> Map.elems solutions

expandFrontier :: Walls -> MinCosts -> Frontier -> (MinCosts, Frontier)
expandFrontier walls minCosts frontier = case Map.minViewWithKey frontier of
  Nothing -> error "empty frontier"
  Just ((cost, bestNodes), frontier') ->
    let newMinCosts =
          Map.difference
            (Map.unionsWith (<>) $ expandNode walls cost <$> bestNodes)
            minCosts
    in ( newMinCosts
       , Map.unionsWith (<>) $ frontier' : [Map.singleton c [n] | (n, Semigroup.Min c) <- Map.toList newMinCosts]
       )

type Node = (Loc, Dir)

type Walls = Set.Set Loc

expandNode :: Walls -> Cost -> Node -> MinCosts
expandNode walls cost (loc, dir) =
  Map.fromList $ concat
    [ [((loc, turnR dir), Semigroup.Min $ cost + 1000) | Set.notMember (loc + dirToV2 (turnR dir)) walls]
    , [((loc, turnL dir), Semigroup.Min $ cost + 1000) | Set.notMember (loc + dirToV2 (turnL dir)) walls]
    , [((loc', dir), Semigroup.Min $ cost + 1) | let loc' = loc ^+^ dirToV2 dir, Set.notMember loc' walls]
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

showSpot :: Maybe Spot -> Char
showSpot = \case
  Just Wl -> '#'
  Nothing -> '.'
  Just St -> 'S'
  Just En -> 'E'

parseChar :: Char -> Maybe Spot
parseChar = \case
  '#' -> Just Wl
  '.' -> Nothing
  'S' -> Just St
  'E' -> Just En
