{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module AdventOfCode4 where

import Data.Functor (($>))
import Control.Applicative ((<|>))
import Data.Char qualified as Char
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Maybe qualified as Maybe
import Text.Read qualified as Read
import Data.Attoparsec.Text qualified as P
import Data.ByteString qualified as BS
import Data.Text qualified as T

fetchInput :: FilePath -> IO Raw
fetchInput = readFile

fetchDay4 :: FilePath -> IO Raw
fetchDay4 = readFile . ("data/input/day4/" <>)

fetchDay4Example :: IO Raw
fetchDay4Example = fetchDay4 "example.txt"

fetchDay4Example2 :: IO Raw
fetchDay4Example2 = fetchDay4 "example2.txt"

fetchDay4Real :: IO Raw
fetchDay4Real = fetchDay4 "real.txt"

type Raw = String
type Line = String
type Lines = [String]

l :: (Show a) => a -> IO ()
--l _x = pure ()
l = print

answer :: (Show a) => a -> IO ()
answer x = do
  putStrLn $ "ANSWER:"
  print x

downRight :: (Int, Int) -> (Int, Int)
downRight (x, y) = (succ x, succ y)

inBounds :: Int -> (Int, Int) -> Bool
inBounds n (x, y) = x >= 0 && x < n && y >= 0 && y < n

diagFrom :: Lines -> (Int, Int) -> Line
diagFrom rows (x0, y0) = [(rows !! y) !! x | (x, y) <- takeWhile (inBounds (length rows)) $ iterate downRight (x0, y0)]

allDiags :: Lines -> Lines
allDiags rows = [diagFrom rows (i, 0) | i <- [0..length rows]] <> [diagFrom rows (0, i) | i <- [1..length rows]]

downLeft :: (Int, Int) -> (Int, Int)
downLeft (x, y) = (pred x, succ y)

gaidFrom :: Lines -> (Int, Int) -> Line
gaidFrom rows (x0, y0) = [(rows !! y) !! x | (x, y) <- takeWhile (inBounds (length rows)) $ iterate downLeft (x0, y0)]

allGaids :: Lines -> Lines
allGaids rows = [gaidFrom rows (i, 0) | i <- [0..maxI]] <> [gaidFrom rows (maxI, i) | i <- [1..maxI]]
 where
  maxI = length rows - 1

findInLine :: Line -> Int
findInLine cs =
  let cs' = dropWhile (\c -> c /= 'S' && c /= 'X') cs
  in case cs' of
    [] -> 0
    'X':'M':'A':'S':cs'' -> 1 + findInLine ('S':cs'')
    'S':'A':'M':'X':cs'' -> 1 + findInLine ('X':cs'')
    _sOrX : cs'' -> findInLine cs''

run1 :: IO ()
run1 = do
  rows <- lines <$> fetchDay4Real
  l rows
  let cols = List.transpose rows
  l cols
  let diags = allDiags rows
  l diags
  let gaids = allGaids rows
  l gaids
  let all = rows <> cols <> diags <> gaids
  let counts = findInLine <$> all
  l counts
  let result = sum counts
  answer result
  -- let result = foldResult instrs
  -- answer result

getAllAIxes :: Lines -> [(Int, Int)]
getAllAIxes rows = [(x, y) | (y, row) <- zip [0..] rows, x <- getRowAIxes row]

getRowAIxes :: Line -> [Int]
getRowAIxes = List.elemIndices 'A'

charAt :: Lines -> (Int, Int) -> Char
charAt rows (x, y) = (rows !! y) !! x

type X = (Char, (Char, Char), (Char, Char))

getX :: Lines -> (Int, Int) -> (Char, (Char, Char), (Char, Char))
getX rows (x, y) = (get (x,y), (get (x-1,y-1), get (x+1,y+1)), (get (x-1,y+1), get (x+1,y-1)))
 where
  get = charAt rows

isXmasAt :: Lines -> (Int, Int) -> Bool
isXmasAt rows (x, y) 
  =  charAt rows (x,y) == 'A'
  && Set.fromList [charAt rows (x-1,y-1), charAt rows (x+1,y+1)] == msSet
  && Set.fromList [charAt rows (x-1,y+1), charAt rows (x+1,y-1)] == msSet

isXmasAtX :: X -> Bool
isXmasAtX (a, (d1,d2), (g1,g2))
  = a == 'A'
  && Set.fromList [d1, d2] == msSet
  && Set.fromList [g1, g2] == msSet

ixInBounds :: Int -> (Int, Int) -> Bool
ixInBounds maxI (x, y)
  =  x >= 1 && x <= maxI - 1
  && y >= 1 && y <= maxI - 1

run2 :: IO ()
run2 = do
  rows <- lines <$> fetchDay4Real
  -- rows <- lines <$> fetchDay4Example
  l rows
  let aIxes = getAllAIxes rows
  l aIxes
  let inBoundIxes = filter (ixInBounds (length rows - 1)) aIxes
  l inBoundIxes
  let xShapes = getX rows <$> inBoundIxes
  l xShapes
  let result = length $ filter isXmasAtX xShapes
  -- let result = length $ filter (isXmasAt rows) inBoundIxes
  answer result

msSet :: Set.Set Char
msSet = Set.fromList "MS"
