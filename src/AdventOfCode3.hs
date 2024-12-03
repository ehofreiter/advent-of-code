{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module AdventOfCode3 where

import Data.Functor (($>))
import Control.Applicative ((<|>))
import Data.Char qualified as Char
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Text.Read qualified as Read
import Data.Attoparsec.Text qualified as P
import Data.ByteString qualified as BS
import Data.Text qualified as T

fetchInput :: FilePath -> IO Raw
fetchInput = readFile

fetchDay3 :: FilePath -> IO Raw
fetchDay3 = readFile . ("data/input/day3/" <>)

fetchDay3Example :: IO Raw
fetchDay3Example = fetchDay3 "example.txt"

fetchDay3Example2 :: IO Raw
fetchDay3Example2 = fetchDay3 "example2.txt"

fetchDay3Real :: IO Raw
fetchDay3Real = fetchDay3 "real.txt"

type Raw = String
type Line = String
type Lines = [String]

linesFromRaw :: Raw -> Lines
linesFromRaw = lines

l :: (Show a) => a -> IO ()
--l _x = pure ()
l = print

answer :: (Show a) => a -> IO ()
answer x = do
  putStrLn $ "ANSWER:"
  print x

type Parser = P.Parser

mulInstrP :: Parser (Int, Int)
mulInstrP = do
  P.string "mul"
  parensP $ do
    a <- numberP
    P.char ','
    b <- numberP
    pure (a, b)

parensP :: Parser a -> Parser a
parensP innerP = do
  P.char '('
  x <- innerP
  P.char ')'
  pure x

numberP :: Parser Int
numberP = do
  ds <- P.takeWhile1 Char.isDigit
  if T.length ds <= 3
    then pure (read $ T.unpack ds)
    else fail "numbers must be 1 to 3 digits"

-- skipThenMul :: Parser (Int, Int)
-- skipThenMul = do
--   P.skipWhile (/= 'm')
--   mulInstrP

mulThenSkip :: Parser (Maybe (Int, Int))
mulThenSkip = do
  mResult <- (Just <$> mulInstrP) <|> (Nothing <$ P.anyChar) -- if mulInstrP fails, skip a character so we don't get stuck on an 'm'
  P.skipWhile (/= 'm')
  pure mResult

mulInstrsP :: Parser [(Int, Int)]
mulInstrsP = Maybe.catMaybes <$> P.many' mulThenSkip

foldResult :: [(Int, Int)] -> Int
foldResult = sum . fmap (uncurry (*))

run1 :: IO ()
run1 = do
  raw <- fetchDay3Real
  Right instrs <- pure $ P.parseOnly mulInstrsP (T.pack raw)
  l instrs
  let result = foldResult instrs
  answer result

histogram :: [Int] -> Map.Map Int Int
histogram ints = Map.fromListWith (+) $ [(i, 1) | i <- ints]

data Instr = Mul Int Int | Do | Don't
  deriving (Eq, Ord, Show)

mulP :: Parser Instr
mulP = uncurry Mul <$> mulInstrP

doP :: Parser Instr
doP = P.string "do()" $> Do

don'tP :: Parser Instr
don'tP = P.string "don't()" $> Don't

instrP :: Parser Instr
instrP = mulP <|> doP <|> don'tP

instrThenSkip :: Parser (Maybe Instr)
instrThenSkip = do
  mResult <- (Just <$> instrP) <|> (Nothing <$ P.anyChar) -- if mulInstrP fails, skip a character so we don't get stuck on an 'm'
  P.skipWhile (\c -> c /= 'm' && c /= 'd')
  pure mResult

instrsP :: Parser [Instr]
instrsP = Maybe.catMaybes <$> P.many' instrThenSkip

process :: Bool -> [Instr] -> [(Int, Int)]
process isDoing = \case
  [] -> []
  instrs@(instr : _) -> case instr of
    Mul x y ->
      let (muls, rest) = span isMul instrs
      in (if isDoing then (Maybe.mapMaybe mulToPair muls) else []) <> process isDoing rest
    Do -> process True (tail instrs)
    Don't -> process False (tail instrs)

isMul :: Instr -> Bool
isMul = \case
  Mul _ _ -> True
  _ -> False

mulToPair :: Instr -> Maybe (Int, Int)
mulToPair = \case
  Mul x y -> Just (x, y)
  Do -> Nothing
  Don't -> Nothing

run2 :: IO ()
run2 = do
  raw <- fetchDay3Real
  let lines = linesFromRaw raw
  l lines
  Right instrs <- pure $ P.parseOnly instrsP (T.pack raw)
  l instrs
  let muls = process True instrs
  l muls
  let result = foldResult muls
  answer result
