module AoC.Y2024.D07.P1 where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text qualified as P
import Data.ByteString qualified as BS
import Data.Char qualified as Char
import Data.Functor
import Data.Functor.WithIndex
import Data.List qualified as List
import Data.List.Split qualified as Split
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.Text qualified as T
import System.FilePath
import Text.Read qualified as Read
import Linear hiding (E)
import Data.Foldable

import AoC.Common
import AoC.CoordVec qualified as CV

day :: Int
day = 7

----------------------------------------

run :: IO ()
run = do
  rows <- lines <$> readReal day
  traverse_ l rows
  let eqns = parseEqn <$> rows
  traverse_ l eqns
  -- let opss = opsForEqn <$> eqns
  -- traverse_ l opss
  -- let evals = eqnEvals <$> eqns
  -- traverse_ l evals
  let valids = filter eqnValid eqns
  -- traverse_ l valids
  let result = sum $ eqResult <$> valids
  answer result

data Eqn = Eqn { eqResult :: Int, eqOperands :: [Int] }
  deriving (Eq, Ord, Show)

parseEqn :: String -> Eqn
parseEqn str =
  let [r, rest] = Split.splitOn ": " str
  in Eqn (read r) (read <$> words rest)

data Op = Add | Mul
  deriving (Eq, Ord, Show, Enum, Bounded)

opsForEqn :: Eqn -> [[Op]]
opsForEqn (Eqn r nums) = sequenceA $ replicate ((length nums) - 1) allOps
 where
  allOps = [Add, Mul]

eqnValid :: Eqn -> Bool
eqnValid eqn@(Eqn r nums) = any (== r) $ eqnEvals eqn

eqnEvals :: Eqn -> [Int]
eqnEvals eqn@(Eqn r nums) =
  let opss = opsForEqn eqn
  in eval nums <$> opss

eval :: [Int] -> [Op] -> Int
eval nums ops = case nums of
  n:ns ->
    let endos = zipWith ap ops ns
        ap o n = case o of
                   Add -> (+ n)
                   Mul -> (* n)
        f r endo = endo r
    in foldl' f n endos
