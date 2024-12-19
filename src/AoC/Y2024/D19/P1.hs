module AoC.Y2024.D19.P1 where

import Control.Applicative ((<|>))
import Control.Lens
import Data.Attoparsec.Text qualified as P
import Data.Bits
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Char qualified as Char
import Data.Either
import Data.Foldable
import Data.Functor
import Data.Functor.WithIndex
import Data.IntSet qualified as IntSet
import Data.List qualified as List
import Data.List.Split qualified as Split
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Traversable
import Linear hiding (E)
import Safe qualified as Safe
import System.FilePath
import Text.Read qualified as Read
import UnliftIO qualified

import AoC.Common
import AoC.CoordVec qualified as CV

type Loc = CV.Coord

day, part :: Int
(day, part) = (19, 1)

----------------------------------------

run :: IO ()
run = do
  printHeader day part
  rows <- fmap BSC.pack . lines <$> readReal day
  -- traverse_ l rows
  let [[patternsStr], designs] = Split.splitOn [""] rows
  let patternSet = Set.fromList $ filter (/= BSC.empty) $ BSC.splitWith (\c -> c == ',' || c == ' ') patternsStr
  T.putStrLn $ "Pattern count: " <> t (Set.size patternSet)
  let patterns = Set.toList patternSet
  -- l patternSet
  -- traverse_ l patterns

  let rps = filter (not . (`isRedundantIn` patternSet)) patterns
  T.putStrLn $ "Refined pattern count: " <> t (length rps)
  traverse_ l rps
  printHoriz

  let bps = getBluePatterns (Set.fromList rps)
  T.putStrLn $ "Blue pattern count: " <> t (Set.size bps)
  traverse_ l $ Set.toList bps
  printHoriz

  solvedDesigns <-
    for designs $ \design -> do
      -- T.putStrLn $ "Design: " <> t design
      solved <- solveDesign bps design
      -- T.putStrLn $ "Solved? " <> if solved then "[X]" else "[ ]"
      -- printHoriz
      pure solved

  let result = length $ filter id solvedDesigns
  answer result

solveDesign :: Set.Set Pattern -> Design -> IO Bool
solveDesign bluePatterns design = do
  let blueIxes = IntSet.toList $ getBlueIndexes design
      ivSet = findIntervals bluePatterns design
  -- T.putStrLn $ "'u' indexes: " <> t blueIxes
  loop blueIxes ivSet
 where
  loop :: [Int] -> Set.Set Iv -> IO Bool
  loop blueIxes ivSet = do
    -- l blueIxes
    -- l ivSet
    case blueIxes of
      [] -> pure True
      bi : bis -> do
        let bivSet = Set.filter (within bi) ivSet
        -- l bivSet
        if Set.null bivSet
          then pure False
          else
            fmap or .
              for (Set.toList bivSet) $ \biv ->
                loop (dropWhile (`within` biv) bis) (Set.filter (not . ivXiv biv) ivSet)

within :: Int -> Iv -> Bool
within ix iv = ivStart iv <= ix && ix < ivEnd' iv

getBluePatterns :: Set.Set Pattern -> Set.Set Pattern
getBluePatterns = Set.filter (BSC.elem 'u')

data Iv = Iv { ivStart :: Int, ivLength :: Int }
  deriving (Eq, Ord, Show)

ivEnd' :: Iv -> Int
ivEnd' iv = ivStart iv + ivLength iv

ivEnds :: Iv -> (Int, Int)
ivEnds iv = (ivStart iv, ivEnd' iv)

-- Are interval (Iv) `a` and interval `b` intersecting/overlapping/crossing ('X'ing)?
ivXiv :: Iv -> Iv -> Bool
ivXiv a b
  | a > b = ivXiv b a
  | otherwise =
      let (a0, a1) = ivEnds a
          (b0, b1) = ivEnds b
      -- a1 == b0 is NOT an intersection
      in a1 > b0

findIntervals :: Set.Set Pattern -> Design -> Set.Set Iv
findIntervals bluePatterns design = Set.fromList $ do -- List
  bp <- Set.toList bluePatterns
  i <- patternIndexes bp design
  pure Iv { ivStart = i, ivLength = BSC.length bp }

getBlueIndexes :: Design -> IntSet.IntSet
getBlueIndexes = IntSet.fromList . BSC.elemIndices 'u'

isRedundantIn :: Pattern -> Set.Set Pattern -> Bool
isRedundantIn p pSet = designIsPossible (Set.delete p pSet) p

type Pattern = BSC.ByteString
type Design = BSC.ByteString

designIsPossible :: Set.Set Pattern -> Design -> Bool
designIsPossible patterns design =
  case Maybe.mapMaybe (`BSC.stripPrefix` design) (Set.toList patterns) of
    [] -> False
    designs'
      | any BSC.null designs' -> True
      | otherwise ->
          or $ designIsPossible patterns <$> designs'

-- idea: make these Vectors or Sequences of Chars instead of T.Text?

-- idea: Use `Seq Char` for Design and Pattern, and try to remove from both ends of Design in each iteration.

-- idea: remove from the middle so we have four ends to work with?

-- idea: delete redundant patterns. e.g. if we have "b" and "bb", this is equivalent to just having
-- "b".

-- we have single-char patterns for "b", "r", "w", and "g". only "u" is lacking a single-char pattern.
-- So only the "u"s matter!

-- idea: get the index that each pattern occurs in a given design
--   Design: wwrwwwuu
--   Pattern: ww is at indexes [0,3,4]
-- Create Map.Map Pattern [Int] for each Design. Each Int `i` represents an
-- interval starting at index `i` and extending `length pattern` characters.
-- Search for disjoint union of these intervals. Existence of a disjoint union
-- means the design is possible.
patternIndexes :: Pattern -> Design -> [Int]
patternIndexes pattern = loop 0
 where
  loop i = \case
    "" -> []
    design -> case BSC.stripPrefix pattern design of
      Nothing -> loop (succ i) (BSC.drop 1 design)
      Just designSuffix -> i : loop (i + BSC.length pattern) designSuffix
