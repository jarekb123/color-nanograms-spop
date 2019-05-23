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

  let columnsConstraints = getColumns constraints
  let rowsConstraints = getRows constraints

  let numOfColumns = length columnsConstraints
  let numOfRows = length rowsConstraints

  putStrLn $ ("Dimension: " ++ show numOfColumns ++ "x" ++ show numOfRows)
  putStrLn $ ("Column constraints: " ++ show columnsConstraints)
  putStrLn $ ("Rows constraints: " ++ show rowsConstraints)

  let nanogram = emptyNanogram numOfColumns numOfRows
  putStrLn $ "--- Empty nanogram ---"
  prettyPrint nanogram

  putStrLn $ "--- Solved nanogram ---"
  let nanogram1 = putColor nanogram Black 0 0
  prettyPrint nanogram1

  return ()