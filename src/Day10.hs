module Day10
    ( 
    day10
   ,day10b
    )
    where

import Data.List (sort, group)
    
day10 :: String -> Int 
day10 input = product $ map length $ group $ sort $ zipWith (-) (tail (0 : a)) (0 : a)
  where a = sort $ parseInput input

day10b :: String -> Int
day10b input = nextAdapter (1 : repeat 0) (sort $ parseInput input) 0
  where 
    nextAdapter [] [] _ = 0
    nextAdapter ys [] _ = head ys
    nextAdapter ys (x : xs) index = nextAdapter ys' xs x
     where diff = x - index -1 
           ys' = foldr (:) ys (sum (take (3 - diff) ys) : replicate diff 0)

parseInput input = (\y-> 3 + maximum y:y ) $ map (\y -> read y::Int) $ lines input
_input = "16\n10\n15\n5\n1\n11\n7\n19\n6\n12\n4"
_input2 = "28\n33\n18\n42\n31\n14\n46\n20\n48\n47\n24\n23\n49\n45\n19\n38\n39\n11\n1\n32\n25\n35\n8\n17\n7\n9\n4\n2\n34\n10\n3"
