module Model where

data Color
  = Empty
  | Black
  | Red
  | Yellow
  | Green
  | Orange
  deriving (Show, Read, Eq)

-- Block - mowi ile kratek danego koloru musi byc zamalowanych w jednym bloku
type Block = (Int, Color)

data Constraints
  = Column [Block]
  | Row [Block]
  deriving (Show, Read)
