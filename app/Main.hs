{-# OPTIONS_GHC -Wno-name-shadowing -Wno-unused-do-bind #-}
module Main where

import Data.Time

import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
--import Day9
import Day10
--import Day11
import Day12
--import Day13
import Day14
import Day15
import Day16
import Day17
import Day18
import Day18b
import Day23

timeStamp :: IO a -> IO ()
timeStamp f = do 
    c <- getCurrentTime                  --  2009-04-21 14:25:29.5585588 UTC 
    print c
    f
    c <- getCurrentTime                  --  2009-04-21 14:25:29.5585588 UTC 
    print c


main :: IO ()
main = 
    timeStamp runDays

runDays :: IO ()
runDays = do
    contents <- readFile "data/day1.txt"
    let answer = show $ day1 contents
    putStrLn ("day1: " ++ answer)
    let answer = show $ day1b contents
    putStrLn ("day1b: " ++ answer)
    contents <- readFile "data/day2.txt"
    let answer = show $ day2 contents
    putStrLn ("day2: " ++ answer)
    let answer = show $ day2b contents
    putStrLn ("day2b: " ++ answer)
    contents <- readFile "data/day3.txt"
    let answer = show $ day3 contents
    putStrLn ("day3: " ++ answer)
    let answer = show $ day3b contents
    putStrLn ("day3b: " ++ answer)

    contents <- readFile "data/day4.txt"
    let answer = show $ day4 contents
    putStrLn ("day4: " ++ answer)
    let answer = show $ day4b contents
    putStrLn ("day4b: " ++ answer)

    contents <- readFile "data/day5.txt"
    let answer = show $ day5 contents 
    putStrLn ("day5: " ++ answer)
    let answer = show $ day5b contents
    putStrLn ("day5b: " ++ answer)

    contents <- readFile "data/day6.txt"
    let answer = show $ day6 contents
    putStrLn ("day6: " ++ answer)
    let answer = show $ day6b contents
    putStrLn ("day6b: " ++ answer)

    contents <- readFile "data/day7.txt"
    let answer = show $ day7 contents
    putStrLn ("day7: " ++ answer)
    let answer = show $ day7b contents
    putStrLn ("day7b: " ++ answer)

    contents <- readFile "data/day8.txt"
    let answer = show $ day8 contents
    putStrLn ("day8: " ++ answer)

    let answer = show $ day8b contents
    putStrLn ("day8b: " ++ answer)

--    contents <- readFile "data/day9.txt"
--    let answer = show $ day9 25 contents 
--    let day9answer = answer
    let answer = "takes too long to run, skipping"
    putStrLn ("day9: " ++ answer)

--    let answer = show $ day9b day9answer contents 
    putStrLn ("day9b: " ++ answer)

    contents <- readFile "data/day10.txt"
    let answer = show $ day10 contents 
    putStrLn ("day10: " ++ answer)
    let answer = show $ day10b contents
    putStrLn ("day10: " ++ answer)

    let answer = "takes too long to run, skipping"
    contents <- readFile "data/day11.txt"
--    let answer = show $ day11 contents 
    putStrLn ("day11: " ++ answer)
--    let answer = show $ day11b contents
    putStrLn ("day11b: " ++ answer)

    contents <- readFile "data/day12.txt"
    let answer = show $ day12 contents 
    putStrLn ("day12: " ++ answer)
    let answer = show $ day12b contents
    putStrLn ("day12b: " ++ answer)

    contents <- readFile "data/day13.txt"
--    let answer = show $ day13 contents 
    let answer = "takes too long to run, skipping"
    putStrLn ("day13: " ++ answer)
--    let answer = show $ day13b contents
    let answer = "takes too long to run, skipping"
    putStrLn ("day13b: " ++ answer)
--
    contents <- readFile "data/day14.txt"
    let answer = show $ day14 contents 
    putStrLn ("day14: " ++ answer)
    let answer = show $ day14b contents
    putStrLn ("day14b: " ++ answer)
    contents <- readFile "data/day15.txt"
    let answer = show $ day15 contents 
    putStrLn ("day15: " ++ answer)
    let answer = "takes too long to run, skipping"
--    let answer = show $ day15b contents
    putStrLn ("day15b: " ++ answer)

    contents <- readFile "data/day16.txt"
    let answer = show $ day16 contents 
    putStrLn ("day16: " ++ answer)
    let answer = show $ day16b contents
    putStrLn ("day16b: " ++ answer)

    contents <- readFile "data/day17.txt"
    let answer = show $ day17 contents 
    putStrLn ("day17: " ++ answer)

    contents <- readFile "data/day18.txt"
    let answer = show $ day18 contents 
    putStrLn ("day18: " ++ answer)
    let answer = show $ day18b contents
    putStrLn ("day18b: " ++ answer)

    let contents = "598162734"
    let answer = show $ day23 100 contents 
    putStrLn ("day23: " ++ answer)
    {-let answer = show $ day23b contents -}
    let answer = "takes too long to run, skipping"
    putStrLn ("day23b: " ++ answer)

----main :: IO ()
--main = someFunc
