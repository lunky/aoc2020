module Day18Spec where

import Day18
import Day18b
import Test.Hspec

spec :: Spec
spec = do
  describe "Day18" $ do
    xit "should do sample 1" $ do
      let input=""
      let expected=0
      day18 input `shouldBe` expected
  describe "Day18b" $ do
    xit "should do sample 1" $ do
      let input=""
      let expected=0
      day18b input `shouldBe` expected
  describe "calculate" $ do
    it "should do sample 1" $ do
      let input="1 + 2 * 3"
      let expected=Just 9
      calculate input `shouldBe` expected
    it "should do sample 2" $ do
      let input="1 + (2 * 3) + (4 * (5 + 6))"
      let expected=Just 51
      calculate input `shouldBe` expected
    it "should do sample 3" $ do
      let input="((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"
      let expected=Just 13632
      calculate input `shouldBe` expected
  describe "calculateb" $ do
    it "should do sample 3" $ do
      let input="((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"
      let expected=Just 23340
      calculateb input `shouldBe` expected
