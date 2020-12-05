module Day5Spec where

import Day5
import Test.Hspec

spec :: Spec
spec = do
  describe "day5" $ 
    it "should run a noop test" $ 
      1 `shouldBe` 1
  describe "day5" $ 
    it "should do sample 1" $ do
      let input = "BFFFBBFRRR\nFFFBBBFRRR\nBBFFBBFRLL"
      let expected = 820
      day5 input `shouldBe` expected
  describe "getRow" $ do
    it "should match pattern 1" $ do 
      let input = "FBFBBFFRLR"
      let expected = 44
      getRow input `shouldBe` expected
    it "should match pattern 2" $ do 
      let input = "BFFFBBFRRR"
      let expected = 70
      getRow input `shouldBe` expected
    it "should match pattern 3" $ do 
      let input = "FFFBBBFRRR"
      let expected = 14
      getRow input `shouldBe` expected
    it "should match pattern 4" $ do 
      let input = "BBFFBBFRLL"
      let expected = 102
      getRow input `shouldBe` expected
  describe "getColumn" $
    it "should match pattern 1" $ do 
      let input = "FBFBBFFRLR"
      let expected = 5
      getColumn input `shouldBe` expected

  describe "getSeatId" $ 
    it "should match pattern 1" $ do 
      let input = "FBFBBFFRLR"
      let expected = 357
      getSeatId input `shouldBe` expected

  describe "getSeatId" $
    it "BFFFBBFRRR: row 70, column 7, seat ID 567" $ do 
      let input = "BFFFBBFRRR"
      let expected = 567
      getSeatId input `shouldBe` expected

  describe "getSeatId" $ 
    it "FFFBBBFRRR: row 14, column 7, seat ID 119" $ do 
      let input = "FFFBBBFRRR"
      let expected = 119
      getSeatId input `shouldBe` expected

  describe "BBFFBBFRLL: row 102, column 4, seat ID 820" $ 
    it "should match pattern 1" $ do 
      let input = "BBFFBBFRLL"
      let expected = 820
      --
      getSeatId input `shouldBe` expected


  describe "day5b" $ 
    it "should do sample 1" $ do
      let input = "BFBBBBBRLL\nBFBBBBBRLR\nBFBBBBBRRL\nBBFFFFFLLL\nBBFFFFFLLR\nBBFFFFFLRL\n"
      let expected = 767
      day5b input `shouldBe` expected
