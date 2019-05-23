module Input where

import           Model

readConstraints :: [String] -> [Constraints]
readConstraints s = map (\x -> read x :: Constraints) s

isColumn :: Constraints -> Bool
isColumn (Column _) = True
isColumn (Row _)    = False

isRow :: Constraints -> Bool
isRow (Row _)    = True
isRow (Column _) = False

getColumnsConstraints = filter isColumn

getRowsConstraints = filter isRow