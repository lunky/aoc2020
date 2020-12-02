module Day1
    (  day1
      ,day1b
    ) where

import Data.List (find, tails)

day1 :: String -> Int
day1 input =  multi $ parseInput input

day1b :: String -> Int
day1b input = product 
                $ head 
                $ filter (\y->sum y==2020) 
                $ combinations 3 
                $ parseInput input


parseInput :: String -> [Int]
parseInput input = map read $ lines input


multi [] = error "shouldn't be here"
multi (x:xs) =  case foundMatch of 
                     Just match  -> x*match
                     Nothing -> multi xs
  where foundMatch=find (\y -> (y + x)==2020) xs 

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = []
combinations 1 l = map (:[]) l
combinations c l = concatMap f $ tails l
    where
            f :: [a] -> [[a]]
            f []     = []
            f (x:xs) = map (x:) $ combinations (c - 1) xs

_input = "1721\n979\n366\n299\n675\n1456"
