module Aoc
(
  countTrue
) where

countTrue :: [Bool] -> Int
countTrue = length . filter (==True)
