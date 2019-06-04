{- Plik ten zawiera algorytm generujacy wszystkie możliwe rozwiązania danego wiersza -}
module Algorithms where

import           Data.List
import           Model
import           Utils

-- Funkcja zliczająca ilość pustych komórek może być jeszcze wstawionych w wierszu/kolumnie
--
-- 1st arg: [Block] - warunki wiersza/kolumny
-- 2nd arg: Int - wielkosc wiersza/kolumny
countEmpty :: [Block] -> Int -> Int
countEmpty [] n          = n
countEmpty ((x, c):bs) n = countEmpty bs (n - x)

-- Funkcja wstawiająca wymagane puste komórki w wierszu/kolumnie.
--
-- NOTE: Puste komórki są wymagane pomiędzy blokami tego samego koloru
addNeededEmptyCells :: [Block] -> [Block]
addNeededEmptyCells [] = []
addNeededEmptyCells [x] = [x]
addNeededEmptyCells ((x1, c1):(x2, c2):xs)
  | c1 == c2 = (x1, c1) : (1, Empty) : (x2, c2) : addNeededEmptyCells xs
  | otherwise = (x1, c1) : addNeededEmptyCells ((x2, c2) : xs)

-- Funkcja wstawiająca elementy z listy we wszystkich możliwych miejscach
--
-- source: https://stackoverflow.com/questions/31288206/how-to-insert-a-list-in-a-list-in-all-possible-ways
interleave :: [a] -> [a] -> [[a]]
interleave xs [] = [xs]
interleave [] ys = [ys]
interleave xs@(x:xs') ys@(y:ys') = map (x :) (interleave xs' ys) ++ map (y :) (interleave xs ys')

-- Funkcja grupująca wszystkie występujące po sobie bloki tego samego koloru w jeden blok
groupBlocks :: [Block] -> [Block]
groupBlocks [] = []
groupBlocks [x] = [x]
groupBlocks ((x1, c1):(x2, c2):xs)
  | c1 == c2 = groupBlocks ((x1 + x2, c1) : xs)
  | otherwise = (x1, c1) : groupBlocks ((x2, c2) : xs)

-- Funkcja generują wszystkie możliwe rozwiązania wiersza/kolumny
--
-- 1st arg: [Block] - warunki wiersza/kolumny
-- 2nd arg: Int -> ilosc komórek w wierszu/kolumnie
allPossibleSolutions :: [Block] -> Int -> [[Block]]
allPossibleSolutions bs n =
  let filledWithNeededEmpty = addNeededEmptyCells bs
      remainingEmptyCells = countEmpty filledWithNeededEmpty n
      remainingEmptyBlocks = generateN (1, Empty) remainingEmptyCells
   -- nub - usuwa duplikaty z listy
   -- (mogą wystąpić przy generowaniu rozwiązań gdy sąsiednie bloki są tego samego koloru - wykryte unit testami)
   in nub (map groupBlocks (interleave remainingEmptyBlocks filledWithNeededEmpty))
