module Main where

import Lib
import Input

main :: IO ()
main = do
  putStrLn "Filename: "
  filename <- getLine
  fileContent <- readFile filename

  putStrLn $ "--- File content ---"
  putStrLn $ fileContent
  putStrLn $ "--- EOF ---"

  let constraints = readConstraints (lines fileContent)

  putStrLn $ "--- Parsed Constraints ---"
  putStrLn $ show constraints

  return ()