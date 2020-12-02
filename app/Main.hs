{-# OPTIONS_GHC -Wno-name-shadowing -Wno-unused-do-bind #-}
module Main where

import Data.Time

import Day1
import Day2

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
--    contents <- readFile "data/day3.txt"
--    let answer = show $ day3 contents
--    putStrLn ("day3: " ++ answer)
--    let answer = show $ day3b contents
--    putStrLn ("day3b: " ++ answer)
--
--    let contents = "382345-843167"
--    let answer = show $ day4 contents
--    putStrLn ("day4: " ++ answer)
--    let answer = show $ day4b contents
--    putStrLn ("day4b: " ++ answer)
--
--    contents <- readFile "data/day5.txt"
--    let answer = show $ day5 contents [1]
--    putStrLn ("day5: " ++ answer)
--
--    contents <- readFile "data/day6.txt"
--    let answer = show $ day6 contents
--    putStrLn ("day6: " ++ answer)
--    let answer = show $ day6b contents
--    putStrLn ("day6b: " ++ answer)
--
--    contents <- readFile "data/day7.txt"
--    let answer = show $ day7 contents
--    putStrLn ("day7: " ++ answer)
--    let answer = show $ day7b contents
--    putStrLn ("day7b: " ++ answer)
--
--    contents <- readFile "data/day8.txt"
--    let answer = show $ day8 (25,6) contents
--    putStrLn ("day8: " ++ answer)
--
--    let answer = day8b (25,6) contents
--    putStrLn ("day8b: \n" ++ answer)
--
--    contents <- readFile "data/day9.txt"
--    let answer = show $ day9 contents [1]
--    putStrLn ("day9: " ++ answer)
--
--    let answer = show $ day9 contents [2]
--    putStrLn ("day9b: " ++ answer)
--
--    contents <- readFile "data/day10.txt"
--    let answer = show $ day10 contents 
--    putStrLn ("day10: " ++ answer)
--    let answer = show $ day10b 199 contents
--    putStrLn ("day10: " ++ answer)
--
--    contents <- readFile "data/day11.txt"
--    let answer = show $ day11 contents 
--    putStrLn ("day11: " ++ answer)
------    let answer = show $ day11b contents
----    putStrLn ("day11: " ++ answer)
--
----main :: IO ()
--main = someFunc
