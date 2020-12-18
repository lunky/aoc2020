module Day13Spec where

import Day13
import Test.Hspec

spec :: Spec
spec = do
  describe "Day13" $ do
    xit "should do sample 1" $ do
      let input = "939\n7,13,x,x,59,x,31,19"
      let expected=295
      day13 input `shouldBe` expected
  describe "Day13b" $ do
    it "should do sample 1" $ do
      let input = "939\n17,x,13,19" 
      let expected=3417
      day13b input `shouldBe` expected
    it "should do sample 2" $ do
      let input = "939\n7,13,x,x,59,x,31,19"
      let expected=1068781
      day13b input `shouldBe` expected
    it "should do sample 3" $ do
      let input = "939\n67,7,59,61"
      let expected=754018
      day13b input `shouldBe` expected
    it "should do sample 4" $ do
      let input = "939\n67,x,7,59,61"
      let expected=779210
      day13b input `shouldBe` expected
    it "should do sample 5" $ do
      let input = "939\n67,7,x,59,61"
      let expected=1261476
      day13b input `shouldBe` expected
    it "should do sample 6" $ do
      let input = "939\n1789,37,47,1889"
      let expected=1202161486
      day13b input `shouldBe` expected
