{-# LANGUAGE MultiWayIf #-}
module AoC.Y2024.D18.P2 where

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
import Data.Ord
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
  let bytes = parseByte <$> rows
      byteCount = length bytes
  -- traverse_ l bytes
  l "Byte count:"
  l byteCount
  -- printHoriz
  let start = V2 0 0
      end = V2 (size-1) (size-1)
      getNBytes n = take n bytes
      findPathAt :: Int -> Maybe [Loc]
      findPathAt n =
        let walls = Set.fromList $ getNBytes n
        in findPath size walls end
      trapIndexes = [1024..byteCount]
  -- for_ (take 10 trapIndexes) $ \i -> do
  --   putStrLn $ show i <> ": " <> show (isTrappedAt i)
  --   printHoriz
  let pm bestPath k = printMap (size, size) bestPath (Set.fromList (getNBytes k)) Nothing
  -- let k = 3450
  -- case findPathAt k of
  --   Nothing -> do
  --     pm [] k
  --     printHoriz
  --     putStrLn "No path found!"
  --   Just bestPath -> do
  --     printMap (size, size) bestPath (Set.fromList (getNBytes k)) Nothing
  --     printHoriz
  --     putStrLn $ "Path found! Length: " <> show (length bestPath)
  let minI = 1024
  (resultI, bestPath) <- searchBounds findPathAt (Maybe.fromJust (findPathAt 1024)) 1024 byteCount
  l resultI
  pm bestPath resultI
  let V2 rx ry = bytes !! resultI
  answer (show rx <> "," <> show ry)

searchBounds :: (Int -> Maybe [Loc]) -> [Loc] -> Int -> Int -> IO (Int, [Loc])
searchBounds getPath paths minI maxI
  | succ minI == maxI =
    pure (minI, paths)
  | otherwise = do
    let i = (maxI + minI) `div` 2
        maybePath = getPath i
    l (minI, maxI, i)
    l maybePath
    case getPath i of
      Nothing -> searchBounds getPath paths minI i
      Just paths' -> searchBounds getPath paths' i maxI

findPath :: Int -> Walls -> Loc -> Maybe [Loc]
findPath size walls end =
  let start = V2 0 0
      allSteps = iterateM (stepState size walls end) $ Right (Map.singleton start [], Map.singleton 0 [start])
  in head $ lefts allSteps

type Cost = Int

type Frontier = Map.Map Cost [Loc]
type BestPaths = Map.Map Loc [Loc]

stepState :: Int -> Walls -> Loc -> (BestPaths, Frontier) -> Either (Maybe [Loc]) (BestPaths, Frontier)
stepState size walls endLoc (bestPaths0, frontier0) =
  case expandFrontier size walls bestPaths0 frontier0 of
    Nothing -> Left Nothing
    Just (newBestPaths, frontier1) ->
      case newBestPaths Map.!? endLoc of
        Nothing -> Right (Map.unionWith (<>) bestPaths0 newBestPaths, frontier1)
        Just bestPath -> Left $ Just bestPath

expandFrontier :: Int -> Walls -> BestPaths -> Frontier -> Maybe (BestPaths, Frontier)
expandFrontier size walls bestPaths frontier =
  Map.minViewWithKey frontier <&> \((cost, bestNodes), frontier') ->
    let newBestPaths =
          Map.difference
            (Map.unionsWith (<>) $ expandPath size walls bestPaths <$> bestNodes)
            bestPaths
    in ( newBestPaths
       , Map.unionsWith (<>) $ frontier' : [Map.singleton (length path) [loc] | (loc, path) <- Map.toList newBestPaths]
       )

type Path = [Loc]

expandPath :: Int -> Walls -> BestPaths -> Loc -> BestPaths
expandPath size walls bestPaths loc =
  Map.fromList
    [ (loc', loc : (bestPaths Map.! loc))
      | dir <- [minBound..maxBound]
      , let loc'@(V2 x' y') = loc ^+^ dirToV2 dir
      , Set.notMember loc' walls
      , x' >= 0 && x' < size
      , y' >= 0 && y' < size
    ]

parseByte :: String -> V2 Int
parseByte s = let [x, y] = Split.splitOn "," s in V2 (read x) (read y)

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

showSpot :: [Loc] -> Walls -> Maybe Loc -> Loc -> Char
showSpot path walls maybeMe pos
  | Set.member pos walls = '#'
  | elem pos path = 'O'
  | maybeMe == Just pos = '@'
  | otherwise = '.'

printMap :: (Int, Int) -> [Loc] -> Walls -> Maybe Loc -> IO ()
printMap size path walls maybeMe = traverse_ putStrLn $ showBestPathLocs size path walls maybeMe

showBestPathLocs :: (Int, Int) -> [Loc] -> Walls -> Maybe Loc -> [String]
showBestPathLocs (sizeX, sizeY) path walls maybeMe =
  [[showSpot path walls maybeMe (V2 x y) | x <- [0..sizeX-1]] | y <- [0..sizeY-1]]
