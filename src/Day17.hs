{-# LANGUAGE TupleSections #-}
module Day17
    (
    day17
   ,day17b
   ,cycle'
   ,iteration
   ,countActiveAdjacent
    )
    where

import Data.Map ()
import qualified Data.Map as Map

day17 :: String -> Int
day17 input = 0

day17b :: String -> Int
day17b input = 0

countActiveAdjacent el set = length
                                $ filter (==Just '#')
                                $ map (`Map.lookup` set) (adjacent el)


cycle' key '#' xs = if countActiveAdjacent key xs==3 || countActiveAdjacent key xs==2 then
                      Map.insert key '#' xs
                    else
                      Map.insert key '.' xs

cycle' key '.' xs = if countActiveAdjacent key xs==3 then Map.insert key '#' xs else Map.insert key '.' xs
cycle' key val acc =  Map.insert key val acc

iteration :: Map.Map (Integer, Integer, Integer) Char -> Map.Map (Integer, Integer, Integer) Char
iteration = Map.foldrWithKey cycle' Map.empty

andAdjacent :: (Foldable t, Num a1, Num a2, Num a3) => t ((a1, a2, a3), b) -> [((a1, a2, a3), Char)]
andAdjacent = concatMap (\(a,b)->map (,'.') $ adjacent a)

parseInput input = concat
                     $ zipWith
                         (\x row -> zipWith (\ y d -> ((x, y, 0), d)) [0 .. ] row)
                            [0 .. ] (lines input)

adjacent (x,y,z) = [
                     (x-1,y-1,z-1),(x,y-1,z-1),(x+1,y-1,z-1),
                     (x-1,y,  z-1),(x,y,z-1),  (x+1,y,  z-1),
                     (x-1,y+1,z-1),(x,y+1,z-1),(x+1,y+1,z-1),

                     (x-1,y-1,  z),(x,y-1,z),  (x+1,y-1,z),
                     (x-1,y,    z),            (x+1,y,  z),
                     (x-1,y+1,  z),(x,y+1,z),  (x+1,y+1,z),

                     (x-1,y-1,z+1),(x,y-1,z+1),(x+1,y-1,z+1),
                     (x-1,y,  z+1),(x,y,  z+1),(x+1,y,  z+1),
                     (x-1,y+1,z+1),(x,y+1,z+1),(x+1,y+1,z+1)
                  ]


_input=".#.\n..#\n###"
