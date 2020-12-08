module Day9Spec where

import Day9
import Test.Hspec

spec :: Spec
spec = do
  describe "Day9" $ do
    xit "should do sample 1" $ do
      let input=""
      let expected=0
      day9 input `shouldBe` expected
  describe "Day9b" $ do
    xit "should do sample 1" $ do
      let input=""
      let expected=0
      day9b input `shouldBe` expected
