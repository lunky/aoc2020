{-# LANGUAGE BangPatterns #-}
module Day23
    ( 
    day23
   ,day23b
--   ,day23b'
   ,move
    )
    where
import Data.Char (digitToInt,intToDigit)
import Data.List (iterate')
import qualified Data.Vector as V

import Control.Monad.ST (ST(..), runST)
import qualified Data.Vector.Mutable as MV

day23 :: Int -> String -> Int 
day23 count input = rearrange 
                        $ V.toList
                        $ iterateN count move 
                        $ V.fromList
                        $ parseInput input 

iterateN num f x = (!! num) $ iterate' f x


{-day23' count input = rearrange $ V.toList $ scoreGame count (parseInput input)-}
  {-where-}
    {-scoreGame cs n = runST $ do-}
      {-iterateN n move' cs-}


test2 :: String -> V.Vector Int
test2 input = runST $ do
  v <- go input            -- call test, which performs the allocation
  t <- V.freeze v
  return t

go :: String -> ST s (MV.MVector s Int)
go input = do
   v <- V.thaw $ V.fromList $ parseInput input
   MV.write v 3 2000    -- modify it
   x <- MV.read v 3     -- access it
   MV.write v 4 (x+1)   
   return v 


day23b :: String -> Int
day23b input = 0

day23b' input = product $ take 2  $ tail $ dropWhile (/=1) 
                        $ V.toList 
                        $ (!! (10^7) )
                        $ iterate' move $ V.fromList $ extendCups $ parseInput input

move vec = insertCups (nextdestCup (front-1) intermediate) theThree intermediate 
  where front = V.head vec
        rest = V.tail vec
        theThree = V.take 3 rest
        intermediate = (V.drop 3 rest) V.++ (V.singleton front)

move' vec = insertCups' (nextdestCup' (front-1) intermediate) theThree intermediate 
  where front = V.head vec
        rest = V.tail vec
        theThree = V.take 3 rest
        intermediate = (V.drop 3 rest) V.++ (V.singleton front)

nextdestCup' x xs = case V.elemIndex x xs of
                        Just y -> y
                        Nothing -> nextdestCup nextIndex xs
  where nextIndex = case x of 
                      0 -> 9
                      y -> y - 1

nextdestCup x xs = case V.elemIndex x xs of
                        Just y -> y
                        Nothing -> nextdestCup nextIndex xs
  where nextIndex = case x of 
                      0 -> 9
                      y -> y - 1

insertCups' x xs xss = beginning  V.++ xs V.++ (V.drop (x+1) xss)
  where beginning = (V.take (x+1) xss) 

insertCups :: Int -> V.Vector a -> V.Vector a -> V.Vector a
insertCups x xs xss = beginning  V.++ xs V.++ (V.drop (x+1) xss)
  where beginning = (V.take (x+1) xss) 

rearrange xs = (\y->read y::Int) $ map intToDigit  (tail $ dropWhile (/=1) xs ++ takeWhile (/=1) xs)

extendCups cups = cups ++ [maximum cups+1..10^6]


parseInput = map digitToInt
_input = "389125467"
_input2 = "54321"
