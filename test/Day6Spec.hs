module Day6Spec where

import Day6
import Test.Hspec

spec :: Spec
spec = do
  describe "day6" $ 
    it "should run a noop test" $ 
      1 `shouldBe` 1
  describe "day6" $ 
    it "should do sample 1" $ do
      let input = "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb\n"
      let expected = 11 
      day6 input `shouldBe` expected
  describe "day6b" $ 
    it "should do sample 1" $ do
      let input = "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb\n"
      let expected = 6
      day6b input `shouldBe` expected
