module Day7Spec where

import Day7
import Test.Hspec

spec :: Spec
spec = do

  describe "Day7" $ 
    it "should do sample 1" $ do
      let input="light red bags contain 1 bright white bag, 2 muted yellow bags.\ndark orange bags contain 3 bright white bags, 4 muted yellow bags.\nbright white bags contain 1 shiny gold bag.\nmuted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\nshiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\ndark olive bags contain 3 faded blue bags, 4 dotted black bags.\nvibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\nfaded blue bags contain no other bags.\ndotted black bags contain no other bags.\n"
      let expected=4
      day7 input `shouldBe` expected

  describe "Day7b" $ 
    it "should do sample 1" $ do
      let input=""
      let expected=0
      day7b input `shouldBe` expected

  describe "Day7b" $ do
    it "should do sample 1" $ do
      let bags=[("light red",[(1,"bright white"),(2,"muted yellow")]),("dark orange",[(3,"bright white"),(4,"muted yellow")]),("bright white",[(1,"shiny gold")]),("muted yellow",[(2,"shiny gold"),(9,"faded blue")]),("shiny gold",[(1,"dark olive"),(2,"vibrant plum")]),("dark olive",[(3,"faded blue"),(4,"dotted black")]),("vibrant plum",[(5,"faded blue"),(6,"dotted black")])]
      let input = (1,"faded blue")
      let expected = 0
      sumBag bags input `shouldBe` expected
    it "should do sample 2" $ do
      let bags=[("light red",[(1,"bright white"),(2,"muted yellow")]),("dark orange",[(3,"bright white"),(4,"muted yellow")]),("bright white",[(1,"shiny gold")]),("muted yellow",[(2,"shiny gold"),(9,"faded blue")]),("shiny gold",[(1,"dark olive"),(2,"vibrant plum")]),("dark olive",[(3,"faded blue"),(4,"dotted black")]),("vibrant plum",[(5,"faded blue"),(6,"dotted black")])]
      let input = (1,"vibrant plum")
      let expected = 11 
      sumBag bags input `shouldBe` expected
    it "should do sample 2" $ do
      let bags=[("light red",[(1,"bright white"),(2,"muted yellow")]),("dark orange",[(3,"bright white"),(4,"muted yellow")]),("bright white",[(1,"shiny gold")]),("muted yellow",[(2,"shiny gold"),(9,"faded blue")]),("shiny gold",[(1,"dark olive"),(2,"vibrant plum")]),("dark olive",[(3,"faded blue"),(4,"dotted black")]),("vibrant plum",[(5,"faded blue"),(6,"dotted black")])]
      let input = (1,"shiny gold")
      let expected = 32
      sumBag bags input `shouldBe` expected
