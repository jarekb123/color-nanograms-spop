module Mappers where

import Model
import Utils

blockToColorArray :: Block -> [Color]
blockToColorArray (n, c) = generateN c n

blocksArrayToColorArray :: [Block] -> [Color]
blocksArrayToColorArray [] = []
blocksArrayToColorArray (b:bs) = (blockToColorArray b) ++ (blocksArrayToColorArray bs)