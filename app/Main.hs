module Main where

import System.Environment
import Lib
import Input
import ConstraintsHelpers
import Model
import Utils
import Algorithms

main :: IO ()
main = do
  --- Nazwa pliku jest brana z argumentow programu
  args <- getArgs
  let filename = args !! 0

  fileContent <- readFile filename

  --- Parsowanie pliku z ograniczeniami nanogramu
  let constraints = readConstraints (lines fileContent)

  putStrLn $ "--- Parsed Input ---"

  let columnsConstraints = getColumnsConstraints constraints
  let rowsConstraints = getRowsConstraints constraints

  let numOfColumns = length columnsConstraints
  let numOfRows = length rowsConstraints

  putStrLn $ ("Dimension: " ++ show numOfColumns ++ "x" ++ show numOfRows)
  putStrLn $ ("Column constraints: " ++ show columnsConstraints)
  putStrLn $ ("Rows constraints: " ++ show rowsConstraints)

  ----- TESTOWANIE POJEDYNCZYCH FUNCKJI ---

  ---
  putStrLn "Test 1"
  let blocks = [(1, Black), (2, Orange)]
  let solutions = allPossibleSolutions blocks 5

  prettyPrint solutions

  putStrLn "Test 2"
  let constraints = [(2, Black), (2, Black)]
  let size = 6

  prettyPrint (allPossibleSolutions constraints size)
--  let nanogram = emptyNanogram numOfColumns numOfRows
--  putStrLn $ "--- Empty nanogram ---"
--  prettyPrint nanogram
--
--  putStrLn $ "--- Testing nanogram ---"
--  let nanogram1 = putColor nanogram Black 1 0
--  let nanogram2 = putColor (putColor (putColor nanogram1 Red 1 1) Black 0 1) Orange 2 1
--  prettyPrint nanogram2
--
--  let row0 = getRow nanogram2 0
--  let row0Constr = rowsConstraints !! 0
--  putStrLn $ ("Row 0: " ++ show row0)
--  putStrLn $ ("Row 0: isMatchingConstraints: " ++ show (isMatchingConstraints row0 row0Constr))
--
--  let row1 = getRow nanogram2 1
--  let row1Constr = rowsConstraints !! 1
--  putStrLn $ ("Row 1: " ++ show row1)
--  putStrLn $ ("Row 1 to blocks: " ++ show (colorsToBlocks row1))
--  putStrLn $ ("Row 1 constraints: " ++ show row1Constr)
--  putStrLn $ ("Row 1: isMatchingConstraints: " ++ show (isMatchingConstraints row1 row1Constr))
--
--  let col0 = getColumn nanogram2 0
--  let col0Constr = columnsConstraints !! 0
--  putStrLn $ ("Col 0: " ++ show col0)
--
--  putStrLn $ ("Col 0: isMatchingConstraints: " ++ show (isMatchingConstraints col0 col0Constr))
--
--  putStrLn $ ("Col 0 & Row 0: isRowColMatchesConstr: " ++
--    show (isColRowMatchesConstr nanogram2 columnsConstraints rowsConstraints 0 0))
--  putStrLn $ ("Col 1 & Row 0: isRowColMatchesConstr: " ++
--    show (isColRowMatchesConstr nanogram2 columnsConstraints rowsConstraints 1 0))

  return ()