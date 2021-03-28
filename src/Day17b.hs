module Day17b
    (
    day17b
   ,tick
   ,lives
   ,GameState(..)
   ,Point(..)
) where

import Data.List (nub,groupBy)
import Data.Map ()
import qualified Data.Map as Map

day17b :: String -> Int
day17b input = length $ day17b' input 6
  
{-# ANN module "HLint: ignore Use !!" #-}
day17b' input count =   (\(GameState state)->state) $
                        head $
                        drop count $
                        iterate tick $ 
                        GameState $ map (Point . fst) $
                        filter(\y->snd y=='#') $ 
                        parseInput input



newtype GameState = GameState [Point] deriving (Show,Eq)
newtype Point = Point (Int,Int,Int,Int) deriving (Show,Eq,Ord)

lives :: Point -> GameState -> Bool
lives point (GameState state)
    | currentlyLiving && (liveNeighbors ==2 || liveNeighbors == 3) = True
    | not currentlyLiving && liveNeighbors ==3 = True
    | otherwise = False
    where neighbors = adjacent point
          liveNeighbors = length $ filter (/=point) $ filter (`elem` state) neighbors
          currentlyLiving = point `elem` state

tick :: GameState -> GameState
tick (GameState gamestate) = GameState (filter (\y -> lives y (GameState gamestate)) gameStateAndAdjacent)
  where gameStateAndAdjacent = nub $ gamestate ++ concatMap adjacent gamestate

parseInput input = concat
                     $ zipWith
                         (\x row -> zipWith (\ y d -> ((0, x, y, 0), d)) [0 .. ] row)
                            [0 .. ] (lines input)
adjacent :: Point -> [Point]
adjacent (Point (w,x,y,z)) = [
                     Point(w-1, x-1,y-1,z-1),Point(w-1,x,y-1,z-1),Point(w-1,x+1,y-1,z-1),
                     Point(w-1, x-1,y,  z-1),Point(w-1,x,y,  z-1),Point(w-1,x+1,y,  z-1),
                     Point(w-1, x-1,y+1,z-1),Point(w-1,x,y+1,z-1),Point(w-1,x+1,y+1,z-1),

                     Point(w-1, x-1,y-1,  z),Point(w-1,x,y-1,z),  Point(w-1,x+1,y-1,z),
                     Point(w-1, x-1,y,    z),Point(w-1,x,y,  z),  Point(w-1,x+1,y,  z),
                     Point(w-1, x-1,y+1,  z),Point(w-1,x,y+1,z),  Point(w-1,x+1,y+1,z),

                     Point(w-1, x-1,y-1,z+1),Point(w-1,x,y-1,z+1),Point(w-1,x+1,y-1,z+1),
                     Point(w-1, x-1,y,  z+1),Point(w-1,x,y,  z+1),Point(w-1,x+1,y,  z+1),
                     Point(w-1, x-1,y+1,z+1),Point(w-1,x,y+1,z+1),Point(w-1,x+1,y+1,z+1),


                     Point(w, x-1,y-1,z-1),  Point(w,x,y-1,z-1),  Point(w,x+1,y-1,z-1),
                     Point(w, x-1,y,  z-1),  Point(w,x,y,  z-1),  Point(w,x+1,y,  z-1),
                     Point(w, x-1,y+1,z-1),  Point(w,x,y+1,z-1),  Point(w,x+1,y+1,z-1),

                     Point(w, x-1,y-1,  z),  Point(w,x,y-1,z),    Point(w,x+1,y-1,z),
                     Point(w, x-1,y,    z),                       Point(w,x+1,y,  z),
                     Point(w, x-1,y+1,  z),  Point(w,x,y+1,z),    Point(w,x+1,y+1,z),

                     Point(w, x-1,y-1,z+1),  Point(w,x,y-1,z+1),  Point(w,x+1,y-1,z+1),
                     Point(w, x-1,y,  z+1),  Point(w,x,y,  z+1),  Point(w,x+1,y,  z+1),
                     Point(w, x-1,y+1,z+1),  Point(w,x,y+1,z+1),  Point(w,x+1,y+1,z+1),


                     Point(w+1, x-1,y-1,z-1),Point(w+1,x,y-1,z-1),Point(w+1,x+1,y-1,z-1),
                     Point(w+1, x-1,y,  z-1),Point(w+1,x,y,  z-1),Point(w+1,x+1,y,  z-1),
                     Point(w+1, x-1,y+1,z-1),Point(w+1,x,y+1,z-1),Point(w+1,x+1,y+1,z-1),

                     Point(w+1, x-1,y-1,  z),Point(w+1,x,y-1,z),  Point(w+1,x+1,y-1,z),
                     Point(w+1, x-1,y,    z),Point(w+1,x,y  ,z),  Point(w+1,x+1,y,  z),
                     Point(w+1, x-1,y+1,  z),Point(w+1,x,y+1,z),  Point(w+1,x+1,y+1,z),

                     Point(w+1, x-1,y-1,z+1),Point(w+1,x,y-1,z+1),Point(w+1,x+1,y-1,z+1),
                     Point(w+1, x-1,y,  z+1),Point(w+1,x,y,  z+1),Point(w+1,x+1,y,  z+1),
                     Point(w+1, x-1,y+1,z+1),Point(w+1,x,y+1,z+1),Point(w+1,x+1,y+1,z+1)
                  ]


_input=".#.\n..#\n###"
