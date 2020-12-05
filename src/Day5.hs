module Day5
    (  day5
      ,day5b
      ,getRow
      ,getColumn
      ,getSeatId
    ) where

import Data.List (sort)

day5 :: String -> Int
day5 input = maximum $ map getSeatId $ parseInput input

day5b :: String -> Int
day5b input =  missingSeatCheck $ sort $ map getSeatId $ parseInput input

getSeatId :: String -> Int
getSeatId input = getRow input * 8 + getColumn input 

getRow :: String -> Int
getRow input = head $ foldr (\y acc -> case y of  'F' -> lowerHalf acc 
                                                  'B' -> upperHalf acc) [0..127] inst 
  where inst = reverse $ take 7 input

getColumn :: String -> Int
getColumn input = head $ foldr (\y acc -> case y of  'L' -> lowerHalf acc 
                                                     'R' -> upperHalf acc) [0..7] inst 
  where inst = reverse $ drop 7 input

upperHalf :: [Int] -> [Int]
upperHalf xs = drop (length xs `div` 2) xs

lowerHalf :: [Int] -> [Int]
lowerHalf xs = take (length xs `div` 2) xs

missingSeatCheck :: [Int] -> Int
missingSeatCheck xs = (\(x,_)->x+1)$ head $ filter (\(x,y)->y-x>1) $ zip xs (tail xs)
  

parseInput :: String -> [String]
parseInput = lines

_input = "FBFBBFFRLR"
