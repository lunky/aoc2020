module Day22Spec where

import Day22
import Test.Hspec

spec :: Spec
spec = do
  describe "Day22" $ do
    xit "should do sample 1" $ do
      let input=""
      let expected=0
      day22 input `shouldBe` expected
  describe "Day22b" $ do
    xit "should do sample 1" $ do
      let input=""
      let expected=0
      day22b input `shouldBe` expected
