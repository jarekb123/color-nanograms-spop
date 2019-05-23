module Model where

import Data.List

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

-- Nanogram - typ reprezentujacy nanogram z zamalowanymi kratkami (boxami)
type Nanogram = [[Color]]

getRow :: Nanogram -> Int -> [Color]
getRow n r = n !! r

getColumn :: Nanogram -> Int -> [Color]
getColumn n c = (transpose n) !! c