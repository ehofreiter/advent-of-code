module AdventOfCode where

import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Text.Read qualified as Read

fetchInput :: FilePath -> IO Raw
fetchInput = readFile

fetchDay1 :: FilePath -> IO Raw
fetchDay1 = readFile . ("data/input/day1/" <>)

fetchDay1Example :: IO Raw
fetchDay1Example = fetchDay1 "part1-example.txt"

fetchDay1Part1 :: IO Raw
fetchDay1Part1 = fetchDay1 "part1.txt"

type Raw = String
type Line = String
type Lines = [String]

linesFromRaw :: Raw -> Lines
linesFromRaw = lines

parsePairs :: Lines -> Maybe [(Int, Int)]
parsePairs = traverse parsePair

parsePair :: Line -> Maybe (Int, Int)
parsePair line = do
  [x, y] <- traverse Read.readMaybe $ words line
  pure (x, y)

listsFromPairs :: [(Int, Int)] -> ([Int], [Int])
listsFromPairs pairs = (fst <$> pairs, snd <$> pairs)

l :: (Show a) => a -> IO ()
--l _x = pure ()
l = print

answer :: (Show a) => a -> IO ()
answer x = do
  putStrLn $ "ANSWER:"
  print x

pairDist :: Int -> Int -> Int
pairDist x y = abs (x - y)

run1 :: IO ()
run1 = do
  raw <- fetchDay1Part1
  let lines = linesFromRaw raw
  l lines
  Just pairs <- pure $ parsePairs lines
  l pairs
  let (lefts_, rights_) = listsFromPairs pairs
  l lefts_
  l rights_
  let (lefts, rights) = (List.sort lefts_, List.sort rights_)
  let dists = zipWith pairDist lefts rights
  let totalDist = sum dists
  answer totalDist

histogram :: [Int] -> Map.Map Int Int
histogram ints = Map.fromListWith (+) $ [(i, 1) | i <- ints]

run2 :: IO ()
run2 = do
  raw <- fetchDay1Part1
  let lines = linesFromRaw raw
  l lines
  Just pairs <- pure $ parsePairs lines
  l pairs
  let (lefts_, rights_) = listsFromPairs pairs
  l (lefts_, rights_)
  let (lefts, rights) = (histogram lefts_, histogram rights_)
  l (lefts, rights)
  let dists = Map.intersectionWith (*) lefts rights
  l dists
  let totalDist = sum $ uncurry (*) <$> Map.toList dists
  answer totalDist
