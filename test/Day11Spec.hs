module Day11Spec where

import qualified Data.Map as Map
import Day11
import Test.Hspec

spec :: Spec
spec = do
  describe "Day11" $ do
    it "should do sample 1" $ do
      let input ="L.LL.LL.LL\nLLLLLLL.LL\nL.L.L..L..\nLLLL.LL.LL\nL.LL.LL.LL\nL.LLLLL.LL\n..L.L.....\nLLLLLLLLLL\nL.LLLLLL.L\nL.LLLLL.LL"
      let expected=37
      day11 input `shouldBe` expected
  describe "Day11b" $ do
    it "should do sample 1" $ do
      let input ="L.LL.LL.LL\nLLLLLLL.LL\nL.L.L..L..\nLLLL.LL.LL\nL.LL.LL.LL\nL.LLLLL.LL\n..L.L.....\nLLLLLLLLLL\nL.LLLLLL.L\nL.LLLLL.LL"
      let expected=26
      day11b input `shouldBe` expected
  describe "sitb" $ do
    it "should not see the #" $ do
      let input = (0,2)
      let input2 = 'L'
      let input3 = Map.fromList $ parseInput "......\n......\n....L#\n......\n......\n......"
      let expected = '#'
      sitb input input2 input3 `shouldBe` expected
    it "should see the #" $ do
      let input = (0,2)
      let input2 = 'L'
      let input3 = Map.fromList $ parseInput "......\n......\n.....#\n......\n......\n......"
      let expected = 'L'
      sitb input input2 input3 `shouldBe` expected
      
