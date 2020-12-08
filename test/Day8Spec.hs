module Day8Spec where

import Day8
import Test.Hspec

spec :: Spec
spec = do
  describe "Day8" $ do
    it "should do sample 1" $ do
      let input="nop +0\nacc +1\njmp +4\nacc +3\njmp -3\nacc -99\nacc +1\njmp -4\nacc +6\n"
      let expected=5
      day8 input `shouldBe` expected
  describe "Day8b" $ do
    xit "should do sample 1" $ do
      let input=""
      let expected=0
      day8b input `shouldBe` expected
