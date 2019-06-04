module AlgorithmsSpec (spec) where

import Test.Hspec
import Algorithms
import Utils
import Model

spec :: Spec
spec = do
  describe "countEmpty" $ do
    it "should return 0 if no empty cells should be in row/cell" $ do
      let constraints = [(1, Black), (2, Orange)]
      let n = 3
      countEmpty constraints n `shouldBe` 0

    it "counts how many empty cells should be in row/cell" $ do
      let constraints = [(1, Black), (1, Orange)]
      let n = 5
      countEmpty constraints n `shouldBe` 3

      let constraints2 = [(1, Black), (2, Orange)]
      countEmpty constraints2 n `shouldBe` 2

  describe "addNeededEmptyCells" $ do
    it "should not add any empty cell if not needed" $ do
      let constraints = [(1, Black), (2, Orange)]
      addNeededEmptyCells constraints `shouldBe` constraints

    it "should add empty cells if needed (1 cell)" $ do
      let constraints = [(1, Black), (2, Black)]
      addNeededEmptyCells constraints `shouldBe` [(1, Black), (1, Empty), (2, Black)]

    it "should add empty cells if needed (2 cells)" $ do
      let constraints = [(1, Black), (2, Black), (1, Orange), (2, Orange)]

      addNeededEmptyCells constraints
        `shouldBe` [(1, Black), (1, Empty), (2, Black), (1, Orange), (1, Empty), (2, Orange)]

  describe "groupBlocks" $ do
    it "should not group blocks with different colors" $ do
      let blocks = [(1, Black), (2, Orange), (3, Empty)]
      groupBlocks blocks `shouldBe` blocks

    it "should group blocks with the same colors" $ do
      let blocks = [(1, Black), (2, Black), (2, Orange), (1, Empty), (1, Empty), (1, Empty), (3, Red)]
      groupBlocks blocks `shouldBe` [(3, Black), (2, Orange), (3, Empty), (3, Red)]

  describe "generateN" $ do
    it "generates n-size list with all the same elements" $ do
      generateN (1, Empty) 4 `shouldBe` [(1, Empty), (1, Empty), (1, Empty), (1, Empty)]

  describe "allPossibleSolutions" $ do
    it "generates all possible solutions of the row/column (different colors)" $ do
      let constraints = [(1, Black), (2, Orange)]
      let size = 5
      let solutions = allPossibleSolutions constraints size

      length solutions `shouldBe` 6

    it "generates all possible solutions of the row/column (with the same colors)" $ do
      let constraints = [(2, Black), (2, Black)]
      let size = 6

      length (allPossibleSolutions constraints size) `shouldBe` 3

