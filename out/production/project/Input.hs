module Input where

-- TODO: Dodac wiecej kolorow ???
data Color = Empty | Black | Red | Yellow | Green | Orange deriving (Show, Read)

-- Block - mowi ile kratek danego koloru musi byc zamalowanych w jednym bloku
type Block = (Int, Color)

data Constraints = Column [Block] | Row [Block] deriving (Show, Read)

readConstraints :: [String] -> [Constraints]
readConstraints s = map (\x -> read x :: Constraints) s

isColumn :: Constraints -> Bool
isColumn (Column _) = True
isColumn (Row _) = False

isRow :: Constraints -> Bool
isRow (Row _) = True
isRow (Column _) = False

getColumns = filter isColumn
getRows = filter isRow