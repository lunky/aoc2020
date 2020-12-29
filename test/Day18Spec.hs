module Day18Spec where

import Day18
import Test.Hspec

spec :: Spec
spec = do
  describe "Day18" $ do
    xit "should do sample 1" $ do
      let input=""
      let expected=0
      day18 input `shouldBe` expected
  describe "Day18b" $ do
    xit "should do sample 1" $ do
      let input=""
      let expected=0
      day18b input `shouldBe` expected
  {-describe "calculate" $ do-}
    {-it "should do sample 1" $ do-}
      {-let input="1 + 2 * 3"-}
      {-let expected=Just 9-}
      {-calculate input `shouldBe` expected-}
