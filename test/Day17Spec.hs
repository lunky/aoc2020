module Day17Spec where

import Day17
import Test.Hspec
import Data.Map ()
import qualified Data.Map as Map

spec :: Spec
spec = do
  describe "Day17" $ do
    xit "should do sample 1" $ do
      let input=".#.\n..#\n###"
      let expected=112
      day17 input `shouldBe` expected
  describe "Day17b" $ do
    xit "should do sample 1" $ do
      let input=""
      let expected=0
      day17b input `shouldBe` expected
  describe "countActiveAdjacent" $ do
    it "should do sample 1" $ do
      let input=(0,0,0)
      let set= Map.fromList [((0,0,0),'#'),((1,0,0),'.')]
      let expected=0
      countActiveAdjacent input set `shouldBe` expected
    it "should do sample 2" $ do
      let input=(0,0,0)
      let set= Map.fromList [((0,0,0),'#'),((1,0,0),'#')]
      let expected=1
      countActiveAdjacent input set `shouldBe` expected
    it "should do sample 3" $ do
      let input=(0,0,0)
      let set= Map.fromList [((0,0,0),'#'),((1,0,0),'#'),((2,2,0),'#')]
      let expected=1
      countActiveAdjacent input set `shouldBe` expected
    it "should do sample 4" $ do
      let input=(0,0,0)
      let set= Map.fromList [((0,0,0),'#'),((1,0,0),'#'),((1,1,1),'#')]
      let expected=2
      countActiveAdjacent input set `shouldBe` expected
