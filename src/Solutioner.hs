module Solutioner where

import           Model
import           Solver
import           Input
import           Algorithms
import           Mappers
import           Utils
import           Data.Maybe
import           Data.List
import           Debug.Trace


-- solveNanogram :: [Constraints] -> Nanogram
-- solveNanogram constraints =
--     let columnsConstraints = getColumnsConstraints constraints
--         rowsConstraints = getRowsConstraints constraints
--         numOfColumns = length columnsConstraints
--         numOfRows = length rowsConstraints
--         nanogram = emptyNanogram numOfColumns numOfRows
--         solve = solveHelper (Just nanogram) columnsConstraints rowsConstraints numOfColumns numOfRows 0 0 
--     in case solve of
--         Just n -> n
--         Nothing -> nanogram

-- solveHelperDebug 

-- nanogram -> ograniczenia kolumn -> ograniczenia wierszy -> liczba kolumn -> liczba wierszy -> aktualny numer wiersza
solveHelper
    :: Maybe Nanogram
    -> [Constraints]
    -> [Constraints]
    -> Int
    -> Int
    -> Int
    -> Maybe Nanogram
solveHelper (Just n) col_const row_const num_col num_row row
    | row == num_row = Just n
    | len > 0 = checkNanogramsDebug n
                                    solutions
                                    col_const
                                    row_const
                                    num_col
                                    num_row
                                    row
    | len == 0 = Nothing
  where
    rowConst  = row_const !! row
    (Row b)   = rowConst
    solutions = allPossibleSolutions b num_col
    len       = length solutions
solveHelper Nothing _ _ _ _ _ = Nothing

checkNanogramsDebug n x col_const row_const num_col num_row row =
    trace ("checkNanograms: " ++ show n ++ "---" ++ show row ++ "---" ++ show x)
        $ checkNanograms n x col_const row_const num_col num_row row

-- Nanogram -> Możliwe rozwiązania wiersza -> ograniczenia kolumn -> ograniczenia wierszy -> liczba kolumn -> liczba wierszy -> aktualny numer wiersza
checkNanograms
    :: Nanogram
    -> [[Block]]
    -> [Constraints]
    -> [Constraints]
    -> Int
    -> Int
    -> Int
    -> Maybe Nanogram
checkNanograms _ [] _ _ _ _ _ = Nothing
checkNanograms n (x : xs) col_const row_const num_col num_row row =
    let row_colors = blocksArrayToColorArray x
        row_nan    = replaceRow n row_colors row
    in  case (checkColConstraintsDebuger row_nan col_const (num_col - 1)) of
            True -> solveHelper (Just row_nan)
                                col_const
                                row_const
                                num_col
                                num_row
                                (row + 1)
            False -> checkNanogramsDebug n
                                         xs
                                         col_const
                                         row_const
                                         num_col
                                         num_row
                                         row


checkColConstraintsDebuger n col_const num_col =
    trace
            (  "checkColConstarints: "
            ++ show n
            ++ "---"
            ++ show col_const
            ++ "---"
            ++ show num_col
            )
        $ checkColConstraints n col_const num_col
-- Nanogram -> ograniczenia kolumny -> liczba kolumn
checkColConstraints :: Nanogram -> [Constraints] -> Int -> Bool
checkColConstraints n col_const 0 =
    let col        = getColumn n 0
        col_constr = col_const !! 0
    in  isMatchingConstraints col col_constr

checkColConstraints n col_const num_col =
    let col        = getColumn n (num_col)
        col_constr = col_const !! (num_col)
    in  (isMatchingConstraints col col_constr)
            && (checkColConstraintsDebuger n col_const (num_col - 1))
