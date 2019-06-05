module SolverSpec (spec) where

import Test.Hspec
import Model
import Solver
import Algorithms
import Mappers

isJust (Just _) = True
isJust Nothing = False

spec :: Spec
spec = do
  describe "isNotExConstr" $ do
    it "should check if all columns/rows does not exceeding constraints (nanogram solved)" $ do
      let columns = [[(1, Black)], [(1, Black), (1, Red), (1, Black)], [(1, Black)]]
      let constr = [[(1, Black)], [(1, Black), (1, Red), (1, Black)], [(1, Black)]]

      isNotExConstr columns constr `shouldBe` True

    it "should check if all columns/rows does not exceeding constraints (nanogram partially solved)" $ do
      let columns = [[(1, Black)], [], [(1, Black)]]
      let constr = [[(1, Black)], [(1, Black), (1, Red), (1, Black)], [(1, Black)]]

      isNotExConstr columns constr `shouldBe` True

    it "should check if all columns/rows does not exceeding constraints (nanogram solved wrong)" $ do
      let columns = [[(1, Black)], [(1, Orange)], [(1, Black)]]
      let constr = [[(1, Black)], [(1, Black), (1, Red), (1, Black)], [(1, Black)]]

      isNotExConstr columns constr `shouldBe` False

  describe "isNgNotExConstr" $ do
    it "should check if nanogram does not exceeding columns constraints (nanogram solved)" $ do
      let ng = [[Empty, Black, Empty], [Black, Red, Black], [Empty, Black, Empty]]
      let constr = [[(1, Black)], [(1, Black), (1, Red), (1, Black)], [(1, Black)]]

      isNgNotExConstr ng 3 constr `shouldBe` True

    it "should check if all columns/rows does not exceeding constraints (nanogram partially solved)" $ do
      let ng = [[Empty, Black, Empty], [Empty, Empty, Empty], [Empty, Black, Empty]]
      let constr = [[(1, Black)], [(1, Black), (1, Red), (1, Black)], [(1, Black)]]

      isNgNotExConstr ng 3 constr `shouldBe` True

    it "should check if all columns/rows does not exceeding constraints (nanogram solved wrong)" $ do
      let ng = [[Empty, Black, Empty], [Empty, Orange, Empty], [Empty, Black, Empty]]
      let constr = [[(1, Black)], [(1, Black), (1, Red), (1, Black)], [(1, Black)]]

      isNgNotExConstr ng 3 constr `shouldBe` False

  describe "solveOne" $ do
    it "should return Just Nanogram if the row is not exceeding columns constraints" $ do
      let ng = []
      let constr = [[(1, Black)], [(1, Black), (1, Red), (1, Black)], [(1, Black)]]

      let (s:ss) = allPossibleSolutions [(1, Black)] 3
      let row = blocksArrayToColorArray [(1, Empty), (1, Black), (1, Empty)]

      solveOne ng 3 row constr `shouldBe` Just [[Empty, Black, Empty]]

    it "should return Nothing if the row is exceeding columns constraints" $ do
      let ng = []
      let constr = [[(1, Black)], [(1, Black), (1, Red), (1, Black)], [(1, Black)]]

      let (s:ss) = allPossibleSolutions [(1, Orange)] 3
      let row = blocksArrayToColorArray s

      solveOne ng 3 row constr `shouldBe` Nothing