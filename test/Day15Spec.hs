module Day15Spec where

import Day15
import Test.Hspec

spec :: Spec
spec = do
  describe "Day15" $ do
    it "should do sample 1" $ do
      let input="0,3,6"
      let expected=436
      day15 input `shouldBe` expected
    it "should do sample 2" $ do
      let input="1,3,2"
      let expected=1
      day15 input `shouldBe` expected
    it "should do sample 3" $ do
      let input="2,1,3"
      let expected=10
      day15 input `shouldBe` expected
    it "should do sample 4" $ do
      let input="1,2,3"
      let expected=27
      day15 input `shouldBe` expected
    it "should do sample 5" $ do
      let input="2,3,1"
      let expected=78
      day15 input `shouldBe` expected
    it "should do sample 7" $ do
      let input="3,2,1"
      let expected=438
      day15 input `shouldBe` expected
    it "should do sample 8" $ do
      let input="3,1,2"
      let expected=1836
      day15 input `shouldBe` expected
  describe "Day15b" $ do
    xit "should do sample 1" $ do
      let input="0,3,6"
      let expected=175594
      day15b input `shouldBe` expected
