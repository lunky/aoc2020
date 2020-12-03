module Day4Spec where

import Day4
import Test.Hspec

spec :: Spec
spec = do
    describe "day4" $ 
        it "should run a noop test" $ 
            1 `shouldBe` 1
    describe "day4" $ 
        it "should do sample 1" $ do
            let input = "..##.......\n#...#...#..\n.#....#..#.\n..#.#...#.#\n.#...##..#.\n..#.##.....\n.#.#.#....#\n.#........#\n#.##...#...\n#...##....#\n.#..#...#.#\n"
            let expected = 0
            day4 input `shouldBe` expected
