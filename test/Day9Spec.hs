module Day9Spec where

import Day9
import Test.Hspec

spec :: Spec
spec = do
  describe "Day9" $ do
    it "should do sample 1" $ do
      let input = "35\n20\n15\n25\n47\n40\n62\n55\n65\n95\n102\n117\n150\n182\n127\n219\n299\n277\n309\n576\n"
      let expected=127
      day9 5 input `shouldBe` expected
  describe "Day9b" $ do
    it "should do sample 1" $ do
      let input = 127
      let input2 = "35\n20\n15\n25\n47\n40\n62\n55\n65\n95\n102\n117\n150\n182\n127\n219\n299\n277\n309\n576\n"
      let expected=62
      day9b input input2 `shouldBe` expected
