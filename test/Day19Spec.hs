module Day19Spec where

import Day19
import Test.Hspec
import Data.IntMap.Strict (IntMap)
import Data.IntMap (fromList)

spec :: Spec
spec = do
  {-describe "parseRules" $ do -}
    {-it "should parse the rules" $ do -}
      {-let input = ["0: 4 1 5","1: 2 3 | 3 2","2: 4 4 | 5 5","3: 4 5 | 5 4","4: \"a\"","5: \"b\""]-}
      {-let expected=fromList [(0,All [Ref 4,Ref 1,Ref 5]),(1,Any [All [Ref 2,Ref 3],All [Ref 3,Ref 2]]),(2,Any [All [Ref 4,Ref 4],All [Ref 5,Ref 5]]),(3,Any [All [Ref 4,Ref 5],All [Ref 5,Ref 4]]),(4,Match 'a'),(5,Match 'b')]-}
      {-parseRules input `shouldBe` expected-}
      
  describe "Day19" $ do
    it "should do sample 1" $ do
      let input="0: 4 1 5\n1: 2 3 | 3 2\n2: 4 4 | 5 5\n3: 4 5 | 5 4\n4: \"a\"\n5: \"b\"\n\nababbb\nbababa\nabbbab\naaabbb\naaaabbb"
      let expected=2
      day19 input `shouldBe` expected
    it "should do content" $ do
      input <- readFile "data/day19.txt"
      let expected=118
      day19 input `shouldBe` expected
  describe "Day19b" $ do
    xit "should do sample 1" $ do
      let input=""
      let expected=0
      day19b input `shouldBe` expected
