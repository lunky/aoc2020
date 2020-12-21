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
    it "should do sample 1" $ do
      let input = "departure class: 0-1 or 4-19\ndeparture row: 0-5 or 8-19\ndeparture seat: 0-13 or 16-19\n\nyour ticket:\n11,12,13\n\nnearby tickets:\n3,9,18\n15,1,5\n5,14,9"
      let expected= 11*12*13
      day16b input `shouldBe` expected
    it "should do sample 2" $ do
      let input = "departure class: 1-3 or 5-7\ndeparture row: 6-11 or 33-44\ndeparture seat: 13-40 or 45-50\n\nyour ticket:\n7,1,14\n\nnearby tickets:\n 3,9,18\n15,1,5\n5,14,9"
      contents <- readFile "data/day16.txt"
      let expected=622670335901
      day16b contents `shouldBe` expected
