module Day3Spec where

import Day3
import Test.Hspec

spec :: Spec
spec = do
    describe "day3" $ 
        it "should run a noop test" $ 
            1 `shouldBe` 1
    describe "day3" $ 
        it "should do sample 1" $ do
            let input = "..##.......\n#...#...#..\n.#....#..#.\n..#.#...#.#\n.#...##..#.\n..#.##.....\n.#.#.#....#\n.#........#\n#.##...#...\n#...##....#\n.#..#...#.#\n"
            let expected = 7
            day3 input `shouldBe` expected
    describe "day3b" $ 
        it "should do sample 1" $ do
            let input = "..##.......\n#...#...#..\n.#....#..#.\n..#.#...#.#\n.#...##..#.\n..#.##.....\n.#.#.#....#\n.#........#\n#.##...#...\n#...##....#\n.#..#...#.#\n"
            let expected = 336
            day3b input `shouldBe` expected
