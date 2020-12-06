module Day6
    (  day6
      ,day6b
    ) where

import Aoc;
import Data.List (nub,sort,group)
import Data.List.Split (splitOn)

day6 :: String -> Int
day6 input = sum $ map (length.nub.concat) $ parseInput input 

day6b :: String -> Int
day6b input =  sum 
  $ map (\y -> countTrue 
                $ map (\c->((length c) >= length y) ) 
                $ group.sort 
                $ concat y ) 
  $ parseInput input

parseInput :: String -> [[String]]
parseInput input = map lines $ splitOn "\n\n" input

_input::String
_input = "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb\n"
