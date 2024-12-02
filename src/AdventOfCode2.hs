{-# LANGUAGE LambdaCase #-}

module AdventOfCode2 where

import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Text.Read qualified as Read

fetchInput :: FilePath -> IO Raw
fetchInput = readFile

fetchDay2 :: FilePath -> IO Raw
fetchDay2 = readFile . ("data/input/day2/" <>)

fetchDay2Example :: IO Raw
fetchDay2Example = fetchDay2 "example.txt"

fetchDay2Real :: IO Raw
fetchDay2Real = fetchDay2 "real.txt"

type Raw = String
type Line = String
type Lines = [String]

linesFromRaw :: Raw -> Lines
linesFromRaw = lines

l :: (Show a) => a -> IO ()
l _x = pure ()
--l = print

answer :: (Show a) => a -> IO ()
answer x = do
  putStrLn $ "ANSWER:"
  print x

type Report = [Int]

type DiffReport = [Int]

diff :: Report -> DiffReport
diff r = zipWith (-) r (tail r)

reportFromLine :: Line -> Report
reportFromLine = fmap read . words

data Score = Score { sMonotonicity :: Monotonicity, sRange :: (Int, Int) }
  deriving (Eq, Ord, Show)

data Monotonicity = Neither | Decreasing | Increasing
  deriving (Eq, Ord, Show)

isMonotonic :: Monotonicity -> Bool
isMonotonic = \case
  Neither -> False
  Decreasing -> True
  Increasing -> True

reportScore :: Report -> Score
reportScore = Score <$> reportMonotonicity <*> reportRange

reportMonotonicity :: Report -> Monotonicity
reportMonotonicity r
  | all (> 0) dr = Increasing
  | all (< 0) dr = Decreasing
  | otherwise = Neither
 where
  dr = diff r

reportRange :: Report -> (Int, Int)
reportRange r = (minimum drAbs, maximum drAbs)
 where
  drAbs = abs <$> diff r

isScoreSafe :: Score -> Bool
isScoreSafe s = isMonotonic (sMonotonicity s) && minDiff >= 1 && maxDiff <= 3
 where
  (minDiff, maxDiff) = sRange s

run1 :: IO ()
run1 = do
  raw <- fetchDay2Real
  let lines = linesFromRaw raw
  l lines
  let reports = reportFromLine <$> lines
  l reports
  let scores = reportScore <$> reports
  l scores
  let safety = isScoreSafe <$> scores
  l safety
  let safetyCount = length $ filter id safety
  answer safetyCount

histogram :: [Int] -> Map.Map Int Int
histogram ints = Map.fromListWith (+) $ [(i, 1) | i <- ints]

isReportSafe2 :: Report -> Bool
isReportSafe2 r = any (isScoreSafe . reportScore) rs
 where
  rs = [take n r <> drop (n + 1) r | n <- [0..length r]]

run2 :: IO ()
run2 = do
  raw <- fetchDay2Real
  let lines = linesFromRaw raw
  l lines
  let reports = reportFromLine <$> lines
  l reports
  let safety = isReportSafe2 <$> reports
  l safety
  let safetyCount = length $ filter id safety
  answer safetyCount
