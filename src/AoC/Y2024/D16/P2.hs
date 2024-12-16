module AoC.Y2024.D16.P2 where

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
import Data.List.NonEmpty qualified as NE
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
(day, part) = (16, 2)

----------------------------------------

run :: IO ()
run = do
  printHeader day part
  rows <- lines <$> readReal day
  traverse_ l rows
  let sm = parseChar <$> CV.fromLists rows
  let spotLocs = locsByType sm
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
          Left c -> (st0, c)
          Right st1 -> loop st1
      ((finalMinCosts, _finalFrontier), bestCost) = loop (minCosts0, frontier0)
  printHoriz
  traverse_ l (Map.toList finalMinCosts)
  printHoriz
  let endMinCosts = [((endLoc, dir), Semigroup.Min bestCost) | dir <- [Rt,Up]]
      bestPathNodeCosts = traceBack walls (Map.union (Map.fromList endMinCosts) finalMinCosts) (Seq.fromList endMinCosts)
  traverse_ l bestPathNodeCosts
  printHoriz
  let bestPathLocs = Set.fromList . fmap (fst . fst) $ toList bestPathNodeCosts
  printBestPathLocs (CV.rowSize sm, CV.colSize sm) walls bestPathLocs
  answer $ Set.size bestPathLocs

type MinCost = Semigroup.Min Cost

traceBack :: Walls -> MinCosts -> Seq.Seq (Node, MinCost) -> Seq.Seq (Node, MinCost)
traceBack walls minCosts = bfs getAdjs
 where
  getAdjs = Set.toList . adjs
  adjs :: (Node, MinCost) -> Set.Set (Node, MinCost)
  adjs ((loc, dir), cost) =
    Set.fromList $ concat
      [ [(n', cost')
          | let n' = (loc - dirToV2 dir, dir)
          , let cost' = cost - 1
          , Map.lookup n' minCosts == Just cost'
        ]
      , [(n', cost')
          | let n' = (loc, turnL dir)
          , let cost' = cost - 1000
          , Map.lookup n' minCosts == Just cost'
        ]
      , [(n', cost')
          | let n' = (loc, turnR dir)
          , let cost' = cost - 1000
          , Map.lookup n' minCosts == Just cost'
        ]
      ]

printBestPathLocs :: (Int, Int) -> Walls -> Set.Set Loc -> IO ()
printBestPathLocs size walls locs = traverse_ putStrLn $ showBestPathLocs size walls locs

showBestPathLocs :: (Int, Int) -> Walls -> Set.Set Loc -> [String]
showBestPathLocs (sizeX, sizeY) walls locs =
  [[toChar (V2 x y) | x <- [0..sizeX-1]] | y <- [0..sizeY-1]]
 where
  toChar loc
    | Set.member loc walls = '#'
    | Set.member loc locs = 'O'
    | otherwise = '.'

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
