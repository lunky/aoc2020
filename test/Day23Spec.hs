module Day23Spec where

import Day23
import Test.Hspec
import qualified Data.Vector as V

spec :: Spec
spec = do
  describe "Day23" $ do
    it "should do sample 1" $ do
      let input = "389125467"
      let expected= 92658374
      day23 10 input `shouldBe` expected
  describe "Day23b" $ do
    xit "should do sample 1" $ do
      let input=""
      let expected=0
      day23b input `shouldBe` expected
  describe "move" $ do
    it "should do move from sample 1" $ do
      let input =    V.fromList [3,8,9,1,2,5,4,6,7]
      let expected = V.fromList [2,8,9,1,5,4,6,7,3]
      move input `shouldBe` expected
    it "should do move from sample 2" $ do
      let input =    V.fromList [2,8,9,1,5,4,6,7,3]
      let expected = V.fromList [5,4,6,7,8,9,1,3,2]
      move input `shouldBe` expected
    it "should do move from sample 3" $ do
      let input =     V.fromList [5,4,6,7,8,9,1,3,2]
      let expected  = V.fromList [8,9,1,3,4,6,7,2,5]
      move input `shouldBe` expected
