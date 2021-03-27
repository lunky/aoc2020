{-# LANGUAGE TupleSections #-}
module Day17
    (
    day17
   ,day17b
   ,tick
   ,lives
--   ,printGameState
   ,GameState(..)
   ,Point(..)
) where

import Data.List (nub,groupBy)
import Data.Map ()
import qualified Data.Map as Map

day17 :: String -> Int
day17 input = length $ day17' input 6
  
day17' input count = (\(GameState state)->state) $
  head$
  drop count $
  iterate tick $ 
  (\y->GameState y) $ map (Point . fst)$ filter(\y->snd y=='#') $ parseInput input

day17b :: String -> Int
day17b input = 0



newtype GameState = GameState [Point] deriving (Show,Eq)
newtype Point = Point (Int,Int,Int) deriving (Show,Eq,Ord)

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

{--
printGameState :: GameState -> IO()
printGameState (GameState state) =  mapM_ 
                                  (putStrLn . map (\y->if Point y `elem`  state then '#' else '.')) 
                                  (groupBy (\x y->fst x==fst y) fullRange)
  where fullRange = [(x,y) | x <- [minXY..maxXY], y<- [minXY..maxXY]]
        flatPoints = concatMap (\(Point (i,j))->[i,j]) state
        minXY = minimum flatPoints
        maxXY = maximum flatPoints
--}

parseInput input = concat
                     $ zipWith
                         (\x row -> zipWith (\ y d -> ((x, y, 0), d)) [0 .. ] row)
                            [0 .. ] (lines input)
adjacent :: Point -> [Point]
adjacent (Point (x,y,z)) = [
                     Point(x-1,y-1,z-1),Point(x,y-1,z-1),Point(x+1,y-1,z-1),
                     Point(x-1,y,  z-1),Point(x,y,  z-1),Point(x+1,y,  z-1),
                     Point(x-1,y+1,z-1),Point(x,y+1,z-1),Point(x+1,y+1,z-1),

                     Point(x-1,y-1,  z),Point(x,y-1,z),  Point(x+1,y-1,z),
                     Point(x-1,y,    z),                 Point(x+1,y,  z),
                     Point(x-1,y+1,  z),Point(x,y+1,z),  Point(x+1,y+1,z),

                     Point(x-1,y-1,z+1),Point(x,y-1,z+1),Point(x+1,y-1,z+1),
                     Point(x-1,y,  z+1),Point(x,y,  z+1),Point(x+1,y,  z+1),
                     Point(x-1,y+1,z+1),Point(x,y+1,z+1),Point(x+1,y+1,z+1)
                  ]


_input=".#.\n..#\n###"
