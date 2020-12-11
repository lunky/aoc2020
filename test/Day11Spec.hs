module Day11Spec where

import Day11
import Test.Hspec

spec :: Spec
spec = do
  describe "Day11" $ do
    xit "should do sample 1" $ do
      let input ="L.LL.LL.LL\nLLLLLLL.LL\nL.L.L..L..\nLLLL.LL.LL\nL.LL.LL.LL\nL.LLLLL.LL\n..L.L.....\nLLLLLLLLLL\nL.LLLLLL.L\nL.LLLLL.LL"
      let expected=37
      day11 input `shouldBe` expected
  describe "Day11b" $ do
    xit "should do sample 1" $ do
      let input=""
      let expected=0
      day11b input `shouldBe` expected
      
