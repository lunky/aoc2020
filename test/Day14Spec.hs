module Day14Spec where

import Day14
import Test.Hspec

spec :: Spec
spec = do
  describe "intToBinary" $ do
    it "should do 1 conversion" $ do
      let input = 132
      let expected = "000000000000000000000000000010000100"
      intToBinary input `shouldBe` expected
    it "should do another conversion" $ do
      let input = 63
      let expected = "000000000000000000000000000000111111"
      intToBinary input `shouldBe` expected
  describe "binaryToInt " $ do
    it "should do 1 conversion" $ do
      let input = "000000000000000000000000000010000100"
      let expected = 132
      binaryToInt input `shouldBe` expected
    it "should do another conversion" $ do
      let input = "000000000000000000000000000000111111"
      let expected = 63
      binaryToInt input `shouldBe` expected
  describe "setMem" $ do
    it "should perform sample 1" $ do
      let value = intToBinary 11
      let mask = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
      let expected = "000000000000000000000000000001001001"
      setMem mask value `shouldBe` expected
    it "should perform sample 2" $ do
      let value = intToBinary 101
      let mask = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
      let expected = "000000000000000000000000000001100101"
      setMem mask value `shouldBe` expected
  describe "Day14" $ do
    it "should do sample 1" $ do
      let input= "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X\nmem[8] = 11\nmem[7] = 101\nmem[8] = 0"
      let expected=165
      day14 input `shouldBe` expected
  describe "Day14b" $ do
    it "should do sample 1" $ do
      let input = "mask = 000000000000000000000000000000X1001X\nmem[42] = 100\nmask = 00000000000000000000000000000000X0XX\nmem[26] = 1"
      let expected=208
      day14b input `shouldBe` expected
