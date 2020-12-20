module Day16Spec where

import Day16
import Test.Hspec

spec :: Spec
spec = do
  describe "Day16" $ do
    it "should do sample 1" $ do
      let input = "class: 1-3 or 5-7\nrow: 6-11 or 33-44\nseat: 13-40 or 45-50\n\nyour ticket:\n7,1,14\n\nnearby tickets:\n7,3,47\n40,4,50\n55,2,20\n38,6,12"
      let expected=71
      day16 input `shouldBe` expected
  describe "Day16b" $ do
    xit "should do sample 1" $ do
      let input=""
      let expected=0
      day16b input `shouldBe` expected
