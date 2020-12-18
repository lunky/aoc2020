{-# LANGUAGE BangPatterns #-}
module Day15
    ( 
    day15
   ,day15b
    )
    where
import Data.List.Split (splitOn)
import Data.Map()
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.List (iterate')

day15 :: String -> Int 
day15 input =(\(a,_,_)->a) $ iterate day15'' 
                  (fst $ last parsedInput, M.fromList parsedInput, length parsedInput)!!(2020-length parsedInput)
  where parsedInput = zip (parseInput input) $ map (:[]) [1..]

day15'' (last,set,idx) = (next, M.insert next (newIdx:nextIdxs) set, newIdx)
        where !lastIdxs = fromMaybe [] (M.lookup last set) 
              next = case lastIdxs of
                      [] -> 0
                      [x] -> idx-x
                      xs -> (\[a,b]->a-b) $ take 2 xs
              newIdx = idx+1
              nextIdxs = take 2 $ fromMaybe [] (M.lookup next set) 

day15b input =  map (\(!a,!b,!c) -> a) (iterate' day15'' 
                  (fst $ last parsedInput, M.fromList parsedInput, length parsedInput)) !! 30000000
  where parsedInput = zip (parseInput input) $ map (:[]) [1..]

parseInput input = map (\y->read y::Int) $ splitOn "," input 
_input = "0,3,6"
