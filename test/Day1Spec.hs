module Day1Spec where

import Day1
import Test.Hspec

spec :: Spec
spec = do
    describe "day1" $ 
        it "should run a noop test" $ 
            1 `shouldBe` 1
    describe "day1" $ do
        it "should do sample 1" $ do
            let input = "1721\n979\n366\n299\n675\n1456"
            let expected = 514579 
            day1 input `shouldBe` expected
        it "should do alternate sample 1" $ do
            let input = "10\n1721\n979\n366\n299\n675\n1456"
            let expected = 514579 
            day1 input `shouldBe` expected
    describe "day1b" $ 
        it "should do sample 1b" $ do
            let input = "1721\n979\n366\n299\n675\n1456"
            let expected = 241861950
            day1b input `shouldBe` expected
