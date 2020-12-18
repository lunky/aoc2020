module Day13
    ( 
    day13
   ,day13b
    )
    where
    
import Data.List.Split (splitOn)
import Data.List (maximumBy)
import Data.Ord (comparing)

day13 :: String -> Int 
day13 input = uncurry (*) $ minimum $ map (\y ->(nextBus estimate y - estimate,y)) buses
  where (estimate, buses) = parseInput input

day13b :: String -> Integer
day13b input = (\y -> nextBusb y (head buses) ) $ head $ filter (`testBuses` buses)  [b-a,b-a+b..]
  where buses = parseInputb input
        largest = uncurry (+) $ maximumBy (comparing snd) buses
        rest = filter (\(a,b)-> b/=largest) buses
        (a,b) = maximumBy (comparing snd) buses

allTheSame :: (Eq a) => [a] -> Bool
allTheSame xs = all (== head xs) (tail xs)

testBuses estimate buses = allTheSame $ map (\y->nextBusb estimate y-fst y) buses

nextBus estimate bus = (estimate `div` bus) * bus 

nextBusb estimate (offset,bus) = if answer==estimate then answer else answer + bus
            where answer = (estimate `div` bus) * bus  

parseInput input = (\(x:xs)->(read x::Int,map (\x->read x::Int) . filter (/="x") . splitOn "," $ head xs) ) $ lines input
parseInputb input = (\(x:xs)-> map (\(a,b)-> (a,read b::Integer) )$ filter (\(_,b) -> b/="x") $ zip [0..] $ splitOn "," $ head xs ) $ lines input
parseInputc input = (\(x:xs)-> map (\(a,b)-> (a,read b::Integer) )$ zipWith (\a b -> (if b == "x" then (a,"0") else (a,b))) [0 .. ] (splitOn "," $ head xs) ) $ lines input
_input = "939\n7,13,x,x,59,x,31,19"


