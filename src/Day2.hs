module Day2
    (  day2
      ,day2b
    ) where

import Data.List.Split
import Aoc

day2 :: String -> Int
day2 input = countTrue $  map day2' $ parseInput input

day2b :: String -> Int
day2b input = countTrue $ map day2b' $ parseInput input



parseInput :: String -> [([Int], Char, String)]
parseInput input = map ((\[x,y,z]-> (map (\y->read y::Int) $ splitOn "-" x,head y, z)) .words) $ lines input


day2' :: Eq a => ([Int], a, [a]) -> Bool
day2' ([min,max],char,pat) = (\y-> y>=min && y<=max) $ length $ filter (==char) pat

day2b' :: ([Int],Char,String)->Bool
day2b' ([min,max], char, patter) = 
  (spot1==char || spot2==char) && not (spot1==char && spot2==char)
            where spot1 = patter!!(min-1)
                  spot2 = patter!!(max-1)

_input = "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc"
