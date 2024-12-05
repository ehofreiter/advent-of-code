{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module AdventOfCode5 where

import Data.Functor (($>))
import Control.Applicative ((<|>))
import Data.Char qualified as Char
import Data.List qualified as List
import Data.List.Split qualified as Split
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Maybe qualified as Maybe
import Text.Read qualified as Read
import Data.Attoparsec.Text qualified as P
import Data.ByteString qualified as BS
import Data.Text qualified as T

fetchInput :: FilePath -> IO Raw
fetchInput = readFile

fetchDay5 :: FilePath -> IO Raw
fetchDay5 = readFile . ("data/input/day5/" <>)

fetchDay5Example :: IO Raw
fetchDay5Example = fetchDay5 "example.txt"

fetchDay5Example2 :: IO Raw
fetchDay5Example2 = fetchDay5 "example2.txt"

fetchDay5Real :: IO Raw
fetchDay5Real = fetchDay5 "real.txt"

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

----------------------------------------

run1 :: IO ()
run1 = do
  rows <- lines <$> fetchDay5Example
  l rows
  [ruleTexts, updateTexts] <- pure $ Split.splitOn [""] rows
  let rules = parseRule <$> ruleTexts
  l rules
  let updates = parseUpdate <$> updateTexts
  l updates
  let valids = validUpdates updates rules
  l valids
  let middlePages = middlePage <$> valids
  l middlePages
  let result = sum middlePages
  answer result

data Rule = R Int Int
  deriving (Eq, Ord, Show)

parseRule :: String -> Rule
parseRule str =
  let [l,r] = Split.splitOn "|" str
  in R (read l) (read r)

type Update = [Page]
type Page = Int

parseUpdate :: String -> Update
parseUpdate str = read <$> Split.splitOn "," str

satisfies :: Update -> Rule -> Bool
satisfies ps (R l r) =
  Maybe.fromMaybe True $
    (<)
      <$> List.elemIndex l ps
      <*> List.elemIndex r ps

checkUpdate :: [Rule] -> Update -> Bool
checkUpdate rs ps = and $ satisfies ps <$> rs

validUpdates :: [Update] -> [Rule] -> [Update]
validUpdates us rs = filter (checkUpdate rs) us

middlePage :: Update -> Page
middlePage ps = ps !! ((length ps - 1) `div` 2)

----------------------------------------

run2 :: IO ()
run2 = do
  rows <- lines <$> fetchDay5Real
  l rows
  [ruleTexts, updateTexts] <- pure $ Split.splitOn [""] rows
  let rules = parseRule <$> ruleTexts
  l rules
  let updates = parseUpdate <$> updateTexts
  l updates
  let invalids = filter (not . checkUpdate rules) updates
  l invalids
  let ordereds = orderUpdate rules <$> invalids
  let middlePages = middlePage <$> ordereds
  l middlePages
  let result = sum middlePages
  answer result

orderUpdate :: [Rule] -> Update -> Update
orderUpdate rs = List.sortBy (rulesCompare rs)

rulesCompare :: [Rule] -> Int -> Int -> Ordering
rulesCompare rs x y
  | R x y `elem` rs = LT
  | R y x `elem` rs = GT
  | otherwise = EQ
