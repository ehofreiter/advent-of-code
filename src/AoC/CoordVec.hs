module AoC.CoordVec
  ( CoordVec
  , Coord
  , flattenCoord
  , unflattenCoord
  , fromLists
  , toLists
  , toList
  , rowSize
  , colSize
  , rowCount
  , colCount
  , allCoords
  , (!)
  , (!?)
  , valid
  , adjs4
  , adjCoords4
  , adjPairs4
  , adjs8
  , adjCoords8
  , adjPairs8
  )
where

import           Data.List.Split
import           Data.Functor.WithIndex
import           Data.Maybe
import qualified Data.Vector as Vec
import           Linear.V2

type Coord = V2 Int
data CoordVec a = CoordVec
  { rowSize :: Int
  , colSize :: Int
  , coordVec :: Vec.Vector a
  }
  deriving (Eq, Show)

instance Functor CoordVec where
  fmap f = overVec (fmap f)

instance FunctorWithIndex (V2 Int) CoordVec where
  imap f cv = overVec (Vec.imap f') cv
    where
      f' i = f (unflattenCoord cv i)

flattenCoord :: CoordVec a -> Coord -> Int
flattenCoord cv (V2 x y) = x + y * colCount cv

unflattenCoord :: CoordVec a -> Int -> Coord
unflattenCoord cv i = V2 x y
  where
    y = i `div` colCount cv
    x = i `mod` colCount cv

-- | Apply a map over the inner Vector. Promise not to change the size!
overVec :: (Vec.Vector a -> Vec.Vector b) -> CoordVec a -> CoordVec b
overVec f cv = cv { coordVec = f (coordVec cv) }

empty :: CoordVec a
empty = CoordVec
  { rowSize = 0
  , colSize = 0
  , coordVec = Vec.empty
  }

singleton :: a -> CoordVec a
singleton x = CoordVec
  { rowSize = 1
  , colSize = 1
  , coordVec = Vec.singleton x
  }

replicate :: Int -> Int -> a -> CoordVec a
replicate numCols numRows x = CoordVec
  { rowSize = numCols
  , colSize = numRows
  , coordVec = Vec.replicate (numCols * numRows) x
  }

fromLists :: [[a]] -> CoordVec a
fromLists [] = CoordVec
  { rowSize = 0
  , colSize = 0
  , coordVec = Vec.empty
  }
fromLists (r:rs) = CoordVec
  { rowSize = length r
  , colSize = length (r:rs)
  , coordVec = Vec.fromList $ concat (r:rs)
  }

toLists :: CoordVec a -> [[a]]
toLists cv = chunksOf (rowSize cv) $ toList cv

toList :: CoordVec a -> [a]
toList = Vec.toList . coordVec

rowCount :: CoordVec a -> Int
rowCount = colSize

colCount :: CoordVec a -> Int
colCount = rowSize

-- |
-- >>> allCoords $ fromLists ["123","456"]
-- [V2 0 0,V2 0 1,V2 1 0,V2 1 1,V2 2 0,V2 2 1]
allCoords :: CoordVec a -> [Coord]
allCoords cv = V2 <$> [0..rowSize cv - 1] <*> [0..colSize cv - 1]

(!) :: CoordVec a -> Coord -> a
(!) cv (V2 x y)
  | x < 0 =
    error $ "Negative col " ++ show x
  | x >= rowSize cv =
    error $ "Col " ++ show x ++ " not less than colCount " ++ show (colCount cv)
  | y < 0 =
    error $ "Negative row " ++ show y
  | y >= colSize cv =
    error $ "Row " ++ show y ++ " not less than rowCount " ++ show (rowCount cv)
  | otherwise =
    coordVec cv Vec.! (x + y * rowSize cv)

(!?) :: CoordVec a -> Coord -> Maybe a
(!?) cv (V2 x y) | x < 0
              || x >= rowSize cv
              || y < 0
              || y >= colSize cv = Nothing
               | otherwise       = coordVec cv Vec.!? (x + y * rowSize cv)

valid :: CoordVec a -> Coord -> Bool
valid cv c = isJust (cv !? c)

leftCol :: CoordVec a -> Int -> Maybe Int
leftCol cv x | x > 0     = Just (x - 1)
             | otherwise = Nothing

rightCol :: CoordVec a -> Int -> Maybe Int
rightCol cv x | x < colCount cv - 1 = Just (x + 1)
              | otherwise           = Nothing

aboveRow :: CoordVec a -> Int -> Maybe Int
aboveRow cv y | y > 0     = Just (y - 1)
              | otherwise = Nothing

belowRow :: CoordVec a -> Int -> Maybe Int
belowRow cv y | y < rowCount cv - 1 = Just (y + 1)
              | otherwise           = Nothing

adjCols :: CoordVec a -> Int -> [Int]
adjCols cv x = catMaybes [leftCol cv x, rightCol cv x]

adjRows :: CoordVec a -> Int -> [Int]
adjRows cv y = catMaybes [aboveRow cv y, belowRow cv y]

adjColCoords :: CoordVec a -> Coord -> [Coord]
adjColCoords cv (V2 x y) = fmap (flip V2 y) $ adjCols cv x

adjRowCoords :: CoordVec a -> Coord -> [Coord]
adjRowCoords cv (V2 x y) = fmap (V2 x) $ adjRows cv y

adjDiagCoords :: CoordVec a -> Coord -> [Coord]
adjDiagCoords cv (V2 x y) = V2 <$> adjCols cv x <*> adjRows cv y

adjCoords4 :: CoordVec a -> Coord -> [Coord]
adjCoords4 cv c = adjColCoords cv c <> adjRowCoords cv c

adjCoords8 :: CoordVec a -> Coord -> [Coord]
adjCoords8 cv c = adjCoords4 cv c <> adjDiagCoords cv c

adjs4 :: CoordVec a -> Coord -> [a]
adjs4 cv c = fmap (cv !) $ adjCoords4 cv c

adjs8 :: CoordVec a -> Coord -> [a]
adjs8 cv c = fmap (cv !) $ adjCoords8 cv c

-- >>> adjPairs4 (fromLists ["ab","cd"]) (V2 0 0)
-- [(V2 1 0,'b'),(V2 0 1,'c')]
-- >>> adjPairs4 (fromLists ["abc","def","ghi"]) (V2 1 1)
-- [(V2 0 1,'d'),(V2 2 1,'f'),(V2 1 0,'b'),(V2 1 2,'h')]
-- >>> adjPairs4 (fromLists ["abc","def","ghi"]) (V2 3 1)
-- [(V2 2 1,'f'),(V2 3 0,*** Exception: Col 3 not less than colCount 3
-- CallStack (from HasCallStack):
--   error, called at /tmp/danteYtE0Hg.hs:115:5 in main:AOC2019.CoordVec
-- >>> adjPairs4 (fromLists ["abc","def","ghi"]) (V2 1 3)
-- [(V2 0 3,*** Exception: Row 3 not less than rowCount 3
-- CallStack (from HasCallStack):
--   error, called at /tmp/danteYtE0Hg.hs:119:5 in main:AOC2019.CoordVec
adjPairs4 :: CoordVec a -> Coord -> [(Coord, a)]
adjPairs4 cv c = fmap f $ adjCoords4 cv c
  where
    f c' = (c', cv ! c')

-- >>> adjPairs8 (fromLists ["ab","cd"]) (V2 0 0)
-- [(V2 1 0,'b'),(V2 0 1,'c'),(V2 1 1,'d')]
-- >>> adjPairs8 (fromLists ["abc","def","ghi"]) (V2 1 1)
-- [(V2 0 1,'d'),(V2 2 1,'f'),(V2 1 0,'b'),(V2 1 2,'h'),(V2 0 0,'a'),(V2 0 2,'g'),(V2 2 0,'c'),(V2 2 2,'i')]
adjPairs8 :: CoordVec a -> Coord -> [(Coord, a)]
adjPairs8 cv c = fmap f $ adjCoords8 cv c
  where
    f c' = (c', cv ! c')
