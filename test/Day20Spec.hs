module Day20Spec where

import Day20
import Test.Hspec

spec :: Spec
spec = do
  describe "Day20" $ do
    xit "should do sample 1" $ do
      let input=""
      let expected=0
      day20 input `shouldBe` expected
  describe "Day20b" $ do
    xit "should do sample 1" $ do
      let input=""
      let expected=0
      day20b input `shouldBe` expected
