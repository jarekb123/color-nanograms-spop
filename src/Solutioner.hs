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


findSolution :: [Constraints] -> Nanogram
findSolution constraints =
    let columnsConstraints = getColumnsConstraints constraints
        rowsConstraints    = getRowsConstraints constraints
        numOfColumns       = length columnsConstraints
        numOfRows          = length rowsConstraints
        nanogram           = emptyNanogram numOfColumns numOfRows
        solutions          = findSolutions columnsConstraints
                                           (numOfColumns - 1)
                                           rowsConstraints
                                           numOfRows
                                           0
                                           [nanogram]
    in  concat solutions


-- ograniczenia kolumn -> liczba kolumn -> ograniczenia wierszy -> liczba wierszy -> który wiersz -> wejściowe nanogramy
findSolutions
    :: [Constraints]
    -> Int
    -> [Constraints]
    -> Int
    -> Int
    -> [Nanogram]
    -> [Nanogram]
findSolutions col_const num_col row_const num_row row nan
    | row == num_row = nan
    | otherwise = findSolutions
        col_const
        num_col
        row_const
        num_row
        (row + 1)
        (findNanograms col_const num_col row_const row nan)

-- ograniczenia kolumn -> liczba kolumn -> ograniczenia wierszy -> który wiersz -> wejściowe nanogramy
findNanograms
    :: [Constraints] -> Int -> [Constraints] -> Int -> [Nanogram] -> [Nanogram]
findNanograms col_const num_col row_const row nan =
    let rowRowConst     = row_const !! row
        (Row rowBlocks) = rowRowConst
        solutions       = allPossibleSolutions rowBlocks (num_col + 1)
    in  findNanogramsFromNanograms col_const num_col row solutions nan

-- ograniczenia kolumn -> liczba kolumn -> który wiersz -> rozwiązania -> nanogramy
findNanogramsFromNanograms
    :: [Constraints] -> Int -> Int -> [[Block]] -> [Nanogram] -> [Nanogram]
findNanogramsFromNanograms col_const num_col row solutions nan =
    let newNanograms =
                (map
                    (findNanogramsFromSolutions col_const num_col row solutions)
                    nan
                )
    in  concat newNanograms

-- ograniczenia kolumn -> liczba kolumn -> który wiersz -> nanogram -> tabela rozwiązań
findNanogramsFromSolutions
    :: [Constraints] -> Int -> Int -> [[Block]] -> Nanogram -> [Nanogram]
findNanogramsFromSolutions col_const num_col row solutions n =
    let newNanograms = (map (makeNanogramFromSolutionDebuger row n) solutions)
        filteredNanograms = filterNanograms newNanograms col_const num_col
    in  filteredNanograms

makeNanogramFromSolutionDebuger row n solution =
    trace
            (  "makeNanogramFromSolutionDebuger: "
            ++ show row
            ++ "---"
            ++ show solution
            ++ "---"
            ++ show n
            )
        $ makeNanogramFromSolution row n solution

-- który wiersz -> rozwiązanie wiersza -> nanogram
makeNanogramFromSolution :: Int -> Nanogram -> [Block] -> Nanogram
makeNanogramFromSolution row n solution =
    let row_colors = blocksArrayToColorArray solution
        n_colors   = replaceRow n row_colors row
    in  n_colors

-- nanogram -> ograniczenia kolumn -> liczba kolumn
filterNanograms :: [Nanogram] -> [Constraints] -> Int -> [Nanogram]
filterNanograms n col_const num_col =
    filter (checkColConstraintsDebuger col_const num_col) n

checkColConstraintsDebuger col_const num_col n =
    trace
            (  "checkColConstarints: "
            ++ show n
            ++ "---"
            ++ show col_const
            ++ "---"
            ++ show num_col
            )
        $ checkColConstraints col_const num_col n

-- Nanogram -> ograniczenia kolumny -> liczba kolumn
checkColConstraints :: [Constraints] -> Int -> Nanogram -> Bool
checkColConstraints col_const 0 n =
    let col        = getColumn n 0
        col_constr = col_const !! 0
    in  isMatchingConstraints col col_constr

checkColConstraints col_const num_col n =
    let col        = getColumn n (num_col)
        col_constr = col_const !! (num_col)
    in  (isMatchingConstraints col col_constr)
            && (checkColConstraintsDebuger col_const (num_col - 1) n)
