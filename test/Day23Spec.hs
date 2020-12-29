module Day23Spec where

import Day23
import Test.Hspec

spec :: Spec
spec = do
  describe "Day23" $ do
    xit "should do sample 1" $ do
      let input = "389125467"
      let expected=0
      day23 input `shouldBe` expected
  describe "Day23b" $ do
    xit "should do sample 1" $ do
      let input=""
      let expected=0
      day23b input `shouldBe` expected
  describe "move" $ do
    it "should do move from sample 1" $ do
      let input =    (0,[3,8,9,1,2,5,4,6,7])
      let expected = (1,[3,2,8,9,1,5,4,6,7])
      move input `shouldBe` expected
    it "should do move from sample 2" $ do
      let input =    (1,[3,2,8,9,1,5,4,6,7])
      let expected = (2,[3,2,5,4,6,7,8,9,1])
      move input `shouldBe` expected
    it "should do move from sample 3" $ do
      let input =     (2,[3,2,5,4,6,7,8,9,1])
      let expected  = (3,[7,2,5,8,9,1,3,4,6])
      move input `shouldBe` expected
