module Day11
    ( 
    day11
   ,day11b
   ,sitb
   ,parseInput
    )
    where

import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
    
day11 :: String -> Int 
day11 input = firstDuplicate  $ map (Map.size . Map.filter (=='#')) 
                              $ iterate doRound 
                              $ Map.fromList 
                              $ parseInput input

firstDuplicate xs = fst.head.dropWhile (uncurry (/=)) $ (\a->zip a (tail a)) xs 

day11b :: String -> Int
day11b input = firstDuplicate $ map (Map.size . Map.filter (=='#')) $ iterate doRoundb $ Map.fromList $ parseInput input


getXY (x,y) grid = case (find (\(xy,_)->xy==(x,y)) grid) of 
                          Just (xy,z) -> Just z
                          _ -> Nothing

getXYMap (x,y) grid = Map.lookup (x,y) grid

adjacent (x,y) grid = map (\y ->getXYMap y grid) [(x-1,y-1),(x,y-1),(x+1,y-1),
                                          (x-1,y  ),        (x+1,y  ),
                                          (x-1,y+1),(x,y+1),(x+1,y+1)]
visible (x,y) grid = map (\z->(takeWhileOneMore (\y -> y /= Just 'L' && y /= Just '#' )) $ map (\y ->getXYMap y grid) z) $ ways (x,y) grid

ways (x,y) grid = 
    [
      --up down
      reverse [(i,y) | i <- [0..x], i/=x],
      [(i,y) | i <- [x..width], i/=x],
      --across
      reverse [(x,i) | i <- [0..y], i/=y],
      [(x,i) | i <- [y..width], i/=y],
      --diagonal down
      [(x-i,y+i) | i <- [1..width], x-i>=0, y+i>=0, x-i<=width, y+i<=width],
      [(x+i,y-i) | i <- [1..width], x+i>=0, y-i>=0, x+i<=width, y-i<=width],
      --diagonal up
      [(i+x,i+y) | i <- [(-1)*width..width], i+x>x, i+y>y, i+x<=width, i+y<=width ],
      reverse [(i+x,i+y) | i <- [(-1)*width..width], i+x>=0, i+y>=0, i+x<x, i+y<y]
    ]
  where 
    width = round $ sqrt $ fromIntegral $ Map.size grid

sit (x,y) seat grid
  | seat=='L' && notElem (Just '#') (adjacent (x,y) grid) = '#'
  | seat=='#' && (>=4) (length $ filter (==Just '#') $ adjacent (x,y) grid) = 'L'
  | otherwise = seat

sitb (x,y) seat grid
  | seat=='L' && (all (==False) $ map (any (==Just '#')) $ visible (x,y) grid) = '#'
  | seat=='#' && (>=5) (length $ filter (==True) $ map (any (==Just '#')) $ visible (x,y) grid) = 'L'
  | otherwise = seat

takeWhileOneMore :: (a -> Bool) -> [a] -> [a]
takeWhileOneMore p = foldr (\x ys -> if p x then x:ys else [x]) []



doRound seats = Map.mapWithKey (\k a ->sit k a seats) seats

doRoundb seats = Map.mapWithKey (\k a ->sitb k a seats) seats

parseInput input =  concatMap (\(y,row) -> map (\(x,datum)-> ((x,y),datum) )  row)
                      $ zip [0..] $ map (zip [0..])
                      $ lines input
_input ="L.LL.LL.LL\nLLLLLLL.LL\nL.L.L..L..\nLLLL.LL.LL\nL.LL.LL.LL\nL.LLLLL.LL\n..L.L.....\nLLLLLLLLLL\nL.LLLLLL.L\nL.LLLLL.LL"
