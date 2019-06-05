module Main where

import System.Environment
import Lib
import Input
import ConstraintsHelpers
import Model
import Utils
import Algorithms
import Solver
import Mappers
import Debug.Trace


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

  let cc = map constraintsToBlocks columnsConstraints
  let (rc:rcs) = map constraintsToBlocks rowsConstraints

  let ng = []
  let rowSolutions = allPossibleSolutions rc 3
  let rowConstr = rcs

  putStrLn $ show (solveDebug ng rowSolutions cc rcs 3)


  let rowSolutions = allPossibleSolutions (rcs !! 0) 3

  putStrLn "-----------"
  putStrLn $ show rowSolutions

  return ()