module MappersSpec (spec) where

import Test.Hspec
import Mappers
import Model

spec :: Spec
spec = do
  describe "blockToColorArray" $ do
    it "maps Block to [Color]" $ do
      blockToColorArray (2, Black) `shouldBe` [Black, Black]

  describe "blocksArrayToColorArray" $ do
      it "maps [Block] to [Color]" $ do
        blocksArrayToColorArray [(2, Black), (4, Empty)] `shouldBe` [Black, Black, Empty, Empty, Empty, Empty]