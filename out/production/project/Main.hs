module Main where

import Lib
import Input

main :: IO ()
main = do
  putStrLn "Filename: "
  filename <- getLine
  fileContent <- readFile filename

  let constraints = readConstraints (lines fileContent)

  putStrLn $ show constraints

  return ()