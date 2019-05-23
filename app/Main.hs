module Main where

import System.Environment
import Lib
import Input
import Solver

main :: IO ()
main = do
  --- Nazwa pliku jest brana z argumentow programu
  args <- getArgs
  let filename = args !! 0

  fileContent <- readFile filename

  --- Parsowanie pliku z ograniczeniami nanogramu
  let constraints = readConstraints (lines fileContent)

  putStrLn $ "--- Parsed Constraints ---"
  putStrLn $ show constraints

  return ()