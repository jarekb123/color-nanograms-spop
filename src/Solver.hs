module Solver where

import           Data.List
import           Model
import           Utils
import Input

emptyNanogram :: Int -> Int -> Nanogram
emptyNanogram cols rows = generate (generate Empty cols) rows

{-|
  Funkcja 'putColor' zamalowuje kratkę (box) nanogramu.
  Przyjmuje trzy argumenty: Nanogram, numer kolumny ('Int'), numer wiersza ('Int').
  Zwraca 'Nanogram' z zamalowaną kratka
-}
putColor :: Nanogram -> Color -> Int -> Int -> Nanogram
putColor n color col row = replaceAt2 n color col row

{-|
  1st arg: constraints
  2nd arg: checking blocks (mapped col/row)

  funkcja sprawdzajaca czy warunki w kolumnie/wierszu sa spelnione

  1. brak warunkow do sprawdzenia, a pozostaly bloki do sprawdzenia -> FALSE
  2. sa warunki do sprawdzenia, a brak pozostalych blokow do sprawdzenia -> TRUE
  3. brak warunkow, brak blokow do sprawdzenia -> TRUE
  4. kolor bloku warunku != kolor bloku sprawdzanego -> sprawdz z nastepnym warunkiem
  5. (kolor bloku warunku == kolor bloku sprawdzanego) && (liczba zamalowanych > max wielkosc bloku) ->
      sprawdz dla nastepnego bloku warunku i aktualnego bloku sprawdzanego
  6. otherwise -> sprawdz dla nastepnego bloku warunku i nastepnego bloku sprawdzanego
-}
isBlocksMatching :: [Block] -> [Block] -> Bool
isBlocksMatching [] [] = True
isBlocksMatching [] (b:bs) = False
isBlocksMatching (b:bs) [] = True
isBlocksMatching ((n1, c1):bs1) ((n2, c2):bs2)
  | c2 /= c1 = isBlocksMatching bs1 ((n2,c2):bs2)
  | (c2 == c1) && (n2 > n1) = isBlocksMatching bs1 ((n2,c2):bs2)
  | otherwise = isBlocksMatching bs1 bs2

-- Funkcja sprawdzająca czy dana kolumna/wiersz spelnia podane warunki
isMatchingConstraints :: [Color] -> Constraints -> Bool
isMatchingConstraints col ctr
  | all (\x -> x == Empty) col = True
  | otherwise =
    let colorsBlocks = colorsToBlocks col
        ctrBlocks = constraintsToBlocks ctr
    in  isBlocksMatching ctrBlocks colorsBlocks

{-|
  Funkcja sprawdzajaca czy podany kolumna i wiersz z nanogramu spelniaja warunki
  1st arg: nanogram
  2nd arg: columns constraints
  3rd arg: rows constraints
  4th arg: column number
  5th arg: row number
-}
isColRowMatchesConstr :: Nanogram -> [Constraints] -> [Constraints] -> Int -> Int -> Bool
isColRowMatchesConstr n cc rc c r =
  let isColumnMatching n cc c =
        let col = getColumn n c
            colConstr = cc !! c
        in  isMatchingConstraints col colConstr
      isRowMatching n rc r =
        let row = getRow n c
            rowConstr = rc !! r
        in  isMatchingConstraints row rowConstr
  in (isColumnMatching n cc c) && (isRowMatching n rc r)


------ Mappers ----
colorsToBlocks :: [Color] -> [Block]
colorsToBlocks c =
  let blocksWithEmpties = map (\x -> (length x, head x)) (group c)
  in  filter (\((x, y)) -> y /= Empty) blocksWithEmpties

constraintsToBlocks :: Constraints -> [Block]
constraintsToBlocks (Column b) = b
constraintsToBlocks (Row b) = b
