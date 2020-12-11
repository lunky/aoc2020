module Day11
    ( 
    day11
   ,day11b
    )
    where

import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
    
day11 :: String -> Int 
day11 input = firstDuplicate $ map (length.filter (=='#').map snd.Map.toList) $ iterate (Day11.round) $ Map.fromList $ parseInput input

firstDuplicate xs = fst.head.dropWhile (uncurry (/=)) $ (\a->zip a (tail a)) xs 

day11b :: String -> Int
day11b input = 0


getXY (x,y) grid = case (find (\(xy,_)->xy==(x,y)) grid) of 
                          Just (xy,z) -> Just z
                          _ -> Nothing

getXYMap (x,y) grid = Map.lookup (x,y) grid

adjacent (x,y) grid = map (\y ->getXYMap y grid) [(x-1,y-1),(x,y-1),(x+1,y-1),
                                          (x-1,y  ),        (x+1,y  ),
                                          (x-1,y+1),(x,y+1),(x+1,y+1)]

sit (x,y) seat grid
  | seat=='L' && notElem (Just '#') (adjacent (x,y) grid) = '#'
  | seat=='#' && (>=4) (length $ filter (==Just '#') $ adjacent (x,y) grid) = 'L'
  | otherwise = seat


round seats = Map.mapWithKey (\k a ->sit k a seats) seats

parseInput input =  concatMap (\(y,row) -> map (\(x,datum)-> ((x,y),datum) )  row)
                      $ zip [0..] $ map (zip [0..])
                      $ lines input
_input ="L.LL.LL.LL\nLLLLLLL.LL\nL.L.L..L..\nLLLL.LL.LL\nL.LL.LL.LL\nL.LLLLL.LL\n..L.L.....\nLLLLLLLLLL\nL.LLLLLL.L\nL.LLLLL.LL"
