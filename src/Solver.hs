module Solver where

import Model
import Algorithms
import Mappers
import ConstraintsHelpers
import Debug.Trace

solveOne :: Nanogram -> Int -> [Color] -> [[Block]] -> Maybe Nanogram
solveOne ng s r c
  | isNgNotExConstr (ng ++ [r]) s c = Just (ng ++ [r])
  | otherwise = Nothing

--solveAll :: Nanogram -> Int -> [[Color]] -> [[Block]] -> Maybe Nanogram

-- sprawdza czy wiersze/kolumny nie przekroczyly ograniczen
-- 1st arg: wiersze/kolumny
-- 2nd arg ograniczenia
isNotExConstr :: [[Block]] -> [[Block]] -> Bool
isNotExConstr [] _ = True
isNotExConstr (b:bs) (c:cs) = isBlocksMatching c b && isNotExConstr bs cs

-- sprawdza czy nanogram nie przekroczyl ograniczen w kolumnach
isNgNotExConstr :: Nanogram -> Int -> [[Block]] -> Bool
isNgNotExConstr ng n c = isNotExConstr (allColumns ng n) c



-------------
---- 1st arg: ograniczenia kolumn
---- 2nd arg: ograniczenia wierszy
---- 3rd arg: ilosc kolumn
---- 4th arg: ilosc wierszy
--solveNanogram :: Nanogram -> [[Block]] -> [[Block]] -> Int -> Int -> Maybe Nanogram
--solveNanogram colCon (r:rs) cn rn =
--  let rowSolutions = allPossibleSolutions r cn
--
--
---- 0th arg: czesciowo rozwiazany nanogram
---- 1st arg: mozliwe rozwiazania wiersza
---- 2nd arg: ograniczenia kolumn
---- 3rd arg: ograniczenia wierszy
---- 4th arg: ilosc kolumn
---- 5th arg: ilosc wierszy
--solveNanogramHelper :: Nanogram -> [[Block]] -> [[Block]] -> [[Block]] -> Int -> Int -> Maybe Nanogram
--solveNanogramHelper ng (sol:solutions) colCon (r:rs) cn rn =
--  let row = blocksArrayToColorArray sol
--  in case solveOne ng cn row colCon of
--    Just newNg ->
--    Nothing ->

solver :: [Constraints] -> [Constraints] -> Int -> Int -> Maybe Nanogram
solver colConstraints rowConstraints colnum rownum =
  let colCon = map constraintsToBlocks colConstraints
      rowCon = map constraintsToBlocks rowConstraints
  in solveNanogram colCon rowCon colnum rownum

solveNanogram :: [[Block]] -> [[Block]] -> Int -> Int -> Maybe Nanogram
solveNanogram colCon (r:rs) cn rn =
  let firstRowSolutions = allPossibleSolutions r cn
  in solve [] firstRowSolutions colCon rs cn

solveDebug :: Nanogram -> [[Block]] -> [[Block]] -> [[Block]] -> Int -> Maybe Nanogram
solveDebug ng sol colConstr rowConstr cn =
  trace (
    "----\n" ++
    "ng = " ++ show ng ++
    "\nsol = \n" ++ show sol ++
    "\nrowConstr = \n" ++ show rowConstr
  ) $ solve ng sol colConstr rowConstr cn

-- czesciowy nanogram -> rozwiazania wiersza -> ogr. kolumn -> ogr. wierszy -> liczba kolumn -> maybe nanogram
solve :: Nanogram -> [[Block]] -> [[Block]] -> [[Block]] -> Int -> Maybe Nanogram
solve ng [] _ [] _  = Nothing
solve ng [] _ _ _  = Nothing

solve ng (s:ss) colCon [] cn =
  let row = blocksArrayToColorArray s
  in case solveOne ng cn row colCon of
    Just newNg -> Just newNg
    Nothing -> solveDebug ng ss colCon [] cn

solve ng (s:ss) colCon (r:rs) cn =
  let row = blocksArrayToColorArray s
  in case solveOne ng cn row colCon of
    Just newNg ->
      let nrs = allPossibleSolutions r cn
      in solveDebug newNg nrs colCon rs cn
    Nothing -> solveDebug ng ss colCon (r:rs) cn

