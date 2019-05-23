module Main where

import System.Environment
import Lib
import Input
import Solver
import Model
import Utils

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

  let nanogram = emptyNanogram numOfColumns numOfRows
  putStrLn $ "--- Empty nanogram ---"
  prettyPrint nanogram

  putStrLn $ "--- Solved nanogram ---"
  let nanogram1 = putColor nanogram Red 0 0
  let nanogram2 = putColor (putColor (putColor nanogram1 Red 1 1) Black 0 1) Black 2 2
  prettyPrint nanogram2

  let row0 = getRow nanogram2 0
  let row0Constr = columnsConstraints !! 0
  putStrLn $ ("Row 0: " ++ show row0)
  putStrLn $ ("Row 0: isMatchingConstraints: " ++ show (isMatchingConstraints row0 row0Constr))

  let row1 = getRow nanogram2 1
  let row1Constr = columnsConstraints !! 1
  putStrLn $ ("Row 1: " ++ show row1)
  putStrLn $ ("Row 1 to blocks: " ++ show (colorsToBlocks row1))
  putStrLn $ ("Row 1: isMatchingConstraints: " ++ show (isMatchingConstraints row1 row1Constr))

  return ()