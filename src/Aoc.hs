module Aoc
(
   countTrue
  ,combinations
) where

import Data.List (tails)

countTrue :: [Bool] -> Int
countTrue = length . filter (==True)

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = []
combinations 1 l = map (:[]) l
combinations c l = concatMap f $ tails l
    where
            f :: [a] -> [[a]]
            f []     = []
            f (x:xs) = map (x:) $ combinations (c - 1) xs

