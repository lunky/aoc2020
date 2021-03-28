module Day17Spec where

import Test.Hspec
import Day17
import Day17b (day17b)
import Data.Map ()
import qualified Data.Map as Map

spec :: Spec
spec = do
  describe "Day17" $ do
    it "should do sample 1" $ do
      let input=".#.\n..#\n###"
      let expected=112
      day17 input `shouldBe` expected
  describe "Day17b" $ do
    it "should do sample 1" $ do
      let input=".#.\n..#\n###"
      let expected=848
      day17b input `shouldBe` expected
  describe "lives" $ do
    it "should determine if a cell should stay alive" $ do
      let input = GameState [Point (0,1,0),Point (1,1,0),Point (2,1,0)]
      let expected = True
      lives (Point (1,1,0)) input `shouldBe` expected
    it "should determine a cell should die" $ do
      let input = GameState [Point (0,1,0),Point (1,1,0),Point (2,1,0)]
      let expected = False
      lives (Point (0,1,0)) input `shouldBe` expected
    it "should determine if a cell come back to life" $ do
      let input = GameState [Point (0,1,0),Point (1,1,0),Point (2,1,0)]
      let expected = True
      lives (Point (1,0,0)) input `shouldBe` expected
  describe "tick" $ do
    it "should match pattern 1" $ do
      let input = GameState [Point (0,1,1),Point (1,1,1),Point (2,1,1)]
      let expected = [  Point (1,0,0), Point (1,1,0), Point (1,2,0),
                        Point (1,0,1), Point (1,1,1), Point (1,2,1),
                        Point (1,0,2), Point (1,1,2), Point (1,2,2)]
      let (GameState output) = tick input 
      output `shouldMatchList` expected

