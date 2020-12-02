module Day2Spec where

import Day2
import Test.Hspec

spec :: Spec
spec = do
    describe "day2" $ 
        it "should run a noop test" $ 
            1 `shouldBe` 1
    describe "day2" $ 
        it "should do sample 1" $ do
            let input = "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc"
            let expected = 2
            day2 input `shouldBe` expected
    describe "day2b" $ 
        it "should do sample 1" $ do
            let input = "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc"
            let expected = 1
            day2b input `shouldBe` expected
