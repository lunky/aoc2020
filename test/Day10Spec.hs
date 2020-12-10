module Day10Spec where

import Day10
import Test.Hspec

spec :: Spec
spec = do
  describe "Day10" $ do
    it "should do sample 1" $ do
      let input = "16\n10\n15\n5\n1\n11\n7\n19\n6\n12\n4"
      let expected=7*5
      day10 input `shouldBe` expected
    it "should do sample 2" $ do
      let input = "28\n33\n18\n42\n31\n14\n46\n20\n48\n47\n24\n23\n49\n45\n19\n38\n39\n11\n1\n32\n25\n35\n8\n17\n7\n9\n4\n2\n34\n10\n3"
      let expected=22*10
      day10 input `shouldBe` expected
  describe "Day10b" $ do
    it "should do sample 1" $ do
      let input = "16\n10\n15\n5\n1\n11\n7\n19\n6\n12\n4"
      let expected=8
      day10b input `shouldBe` expected
    it "should do sample 2" $ do
      let input = "28\n33\n18\n42\n31\n14\n46\n20\n48\n47\n24\n23\n49\n45\n19\n38\n39\n11\n1\n32\n25\n35\n8\n17\n7\n9\n4\n2\n34\n10\n3"
      let expected=19208
      day10b input `shouldBe` expected
