module Day19Spec where

import Day19
import Test.Hspec

spec :: Spec
spec = do
  describe "Day19" $ do
    xit "should do sample 1" $ do
      let input=""
      let expected=0
      day19 input `shouldBe` expected
  describe "Day19b" $ do
    xit "should do sample 1" $ do
      let input=""
      let expected=0
      day19b input `shouldBe` expected
