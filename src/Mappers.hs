module Mappers where

import           Model
import           Utils
import           Data.List


blockToColorArray :: Block -> [Color]
blockToColorArray (n, c) = generateN c n

blocksArrayToColorArray :: [Block] -> [Color]
blocksArrayToColorArray [] = []
blocksArrayToColorArray (b:bs) = (blockToColorArray b) ++ (blocksArrayToColorArray bs)

------ Mappers ----
colorsToBlocks :: [Color] -> [Block]
colorsToBlocks c =
  let blocksWithEmpties = map (\x -> (length x, head x)) (group c)
   in filter (\((x, y)) -> y /= Empty) blocksWithEmpties

constraintsToBlocks :: Constraints -> [Block]
constraintsToBlocks (Column b) = b
constraintsToBlocks (Row b)    = b

allColumns :: [[Color]] -> Int -> [[Block]]
allColumns ng 1 = [colorsToBlocks (getColumn ng 0)]
allColumns ng n = allColumns ng (n - 1) ++ [colorsToBlocks (getColumn ng (n - 1))]
