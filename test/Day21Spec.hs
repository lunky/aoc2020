module Day21Spec where

import Day21
import Test.Hspec

spec :: Spec
spec = do
  describe "Day21" $ do
    xit "should do sample 1" $ do
      let input=""
      let expected=0
      day21 input `shouldBe` expected
  describe "Day21b" $ do
    xit "should do sample 1" $ do
      let input=""
      let expected=0
      day21b input `shouldBe` expected
