module Solver where

import Model
import Algorithms
import Mappers
import ConstraintsHelpers

-- Funkcja rozwiązująca nanogram
--
-- 1st arg: mozliwe rozwiazania wiersza
-- 2nd arg: warunki dla wierszow
-- 3rd arg: warunki dla kolumn
-- 4th arg: liczba kolumn
--solve :: [[Block]] -> [[Block]] -> Int -> Maybe Nanogram
--solve (r:rs) rc cc n =
--  let row = blockToColorArray r



-- sprawdza czy wiersze/kolumny nie przekroczyly ograniczen
-- 1st arg: wiersze/kolumny
-- 2nd arg ograniczenia
isNotExConstr :: [[Block]] -> [[Block]] -> Bool
isNotExConstr [] _ = True
isNotExConstr (b:bs) (c:cs) = isBlocksMatching c b && isNotExConstr bs cs

-- sprawdza czy nanogram nie przekroczyl ograniczen w kolumnach
isNgNotExConstr :: Nanogram -> Int -> [[Block]] -> Bool
isNgNotExConstr ng n c = isNotExConstr (allColumns ng n) c