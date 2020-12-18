module Day12Spec where

import Day12
import Test.Hspec

spec :: Spec
spec = do
  describe "Day12" $ do
    it "should do sample 1" $ do
      let input = "F10\nN3\nF7\nR90\nF11\n"
      let expected=25
      day12 input `shouldBe` expected
  describe "Day12b" $ do
    it "should do sample 1" $ do
      let input = "F10\nN3\nF7\nR90\nF11\n"
      let expected=286
      day12b input `shouldBe` expected
