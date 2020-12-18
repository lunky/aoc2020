module Day12
    ( 
    day12
   ,day12b
    )
    where
import Data.List (foldl')

type Location = (Int, Int)
type Operation = (Location, Location) -> Char -> Int -> (Location, Location)

    
day12 :: String -> Int  
day12 = distance addToBoatLocation (1, 0)
  where addToBoatLocation (t, dt) inst mag = (addToLocation t inst mag, dt)

day12b :: String -> Int
day12b = distance addToWaypointLocation (10, 1)
  where addToWaypointLocation (t, dt) inst mag = (t, addToLocation dt inst mag)

rotateLeft :: Location -> Int -> Location
rotateLeft (x, y) 90 = (-1 * y, x)
rotateLeft t x
  | x `mod` 90 == 0 = rotateLeft (rotateLeft t 90) (x - 90)
  | otherwise = error $ "shouldn't be here" ++ show x

addToLocation :: Location -> Char -> Int -> Location
addToLocation (x,y) inst mag
  | inst == 'N' = (x, y + mag)
  | inst == 'S' = (x, y - mag)
  | inst == 'E' = (x + mag, y)
  | inst == 'W' = (x - mag, y)
  | otherwise = error $ "shouldn't be here" ++ [inst]

instruction :: Operation -> (Location, Location) -> String -> (Location, Location)
instruction operation ((x,y), (dx, dy)) (inst:magStr)
  | inst == 'F' = ((x + dx * mag, y + dy * mag), (dx, dy))
  | inst == 'L' = ((x, y), rotateLeft (dx, dy) mag)
  | inst == 'R' = ((x, y), rotateLeft (dx, dy) (360 - mag))
  | otherwise = operation ((x, y), (dx, dy)) inst mag
  where mag = read magStr
instruction _ _ s = error $ "shouldn't be here: " ++ s

distance :: Operation -> Location -> String -> Int
distance operation initialWaypoint =
  (\((x,y),_) -> abs x + abs y) . foldl' (instruction operation ) ((0,0), initialWaypoint) . lines
