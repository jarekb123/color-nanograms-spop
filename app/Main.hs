module Main where

import           System.Environment
import           Lib
import           Input
import           Solver
import           Model
import           Utils
import           Algorithms
import           Solutioner

check :: Maybe Nanogram -> String
check Nothing  = "false"
check (Just n) = "true"

checkNanogram :: Maybe Nanogram -> Nanogram
checkNanogram Nothing  = emptyNanogram 4 4
checkNanogram (Just n) = n

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
  let rowsConstraints    = getRowsConstraints constraints

  let numOfColumns       = length columnsConstraints
  let numOfRows          = length rowsConstraints

  putStrLn $ ("Dimension: " ++ show numOfColumns ++ "x" ++ show numOfRows)
  putStrLn $ ("Column constraints: " ++ show columnsConstraints)
  putStrLn $ ("Rows constraints: " ++ show rowsConstraints)

  ----- TESTOWANIE POJEDYNCZYCH FUNCKJI ---

  ---
  putStrLn "Test 1"
  let blocks    = [(1, Black)]
  let solutions = allPossibleSolutions blocks 3

  prettyPrint solutions

  -- putStrLn "Test 2"
  -- let constraints = [(2, Black), (2, Black)]
  -- let size = 6

  -- prettyPrint (allPossibleSolutions constraints size)

  let nanogram = emptyNanogram numOfColumns numOfRows
  putStrLn $ ("--- Empty nanogram ---")
  prettyPrint nanogram

  putStrLn $ ("--- Testing nanogram ---")
  let nanogram1 = putColor nanogram Black 1 0
  let nanogram2 =
        putColor (putColor (putColor nanogram1 Red 1 1) Black 0 1) Orange 2 1
  prettyPrint nanogram2

  putStrLn $ ("--- Testing replaceRow ---")

  let row1 = [Black, Red, Black]
  let row2 = [Empty, Empty, Empty]

  let nan1 = replaceRow nanogram row1 0
  let nan  = replaceRow nan1 row2 2

  prettyPrint nan


  putStrLn $ "--- Testing solutioner ---"
  let const_1 = checkColConstraints nan columnsConstraints (numOfColumns - 1)
  putStrLn $ (if const_1 then "true" else "false")

  putStrLn $ "--- Testing checkNanograms ---"

  let checkNano = checkNanograms nanogram
                                 solutions
                                 columnsConstraints
                                 rowsConstraints
                                 numOfColumns
                                 numOfRows
                                 0
  let ioNano = check checkNano
  putStrLn $ ioNano

  putStrLn $ "--- Testing solve helper ---"
  -- solveHelper :: Maybe Nanogram -> [Constraints] -> [Constraints] -> Int -> Int -> Int -> Maybe Nanogram
  let eNanogram = emptyNanogram numOfColumns numOfRows

  -- let solveNan = solveHelper (Just eNanogram)
  --                            columnsConstraints
  --                            rowsConstraints
  --                            numOfColumns
  --                            numOfRows
  --                            0
  -- let solvedNanogram = checkNanogram solveNan
  -- prettyPrint solvedNanogram

  putStrLn $ show
    (solveHelper (Just eNanogram)
                 columnsConstraints
                 rowsConstraints
                 numOfColumns
                 numOfRows
                 0
    )


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
