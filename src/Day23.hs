module Day23
    ( 
    day23
   ,day23b
   ,move
   ,mv
    )
    where

import Data.Char (digitToInt,intToDigit)
import Data.List (iterate')
import qualified Data.Vector as V

import Control.Monad
import Control.Monad.ST (ST(..), runST)
import qualified Data.Vector.Mutable as MV
import Data.Vector ((!))

day23 :: Int -> String -> Int 
day23 count input = rearrange 
                        $ V.toList
                        $ iterateN count move 
                        $ V.fromList
                        $ parseInput input 

iterateN num f x = (!! num) $ iterate' f x

move vec = insertCups (nextdestCup (front-1) intermediate) theThree intermediate 
  where front = V.head vec
        rest = V.tail vec
        theThree = V.take 3 rest
        intermediate = V.drop 3 rest V.++ V.singleton front

nextdestCup x xs = case V.elemIndex x xs of
                        Just y -> y
                        Nothing -> nextdestCup nextIndex xs
  where nextIndex = case x of 
                      0 -> 9
                      y -> y - 1

insertCups :: Int -> V.Vector a -> V.Vector a -> V.Vector a
insertCups x xs xss = beginning  V.++ xs V.++ V.drop (x+1) xss
  where beginning = V.take (x+1) xss 

rearrange xs = (\y->read y::Int) $ map intToDigit  (tail $ dropWhile (/=1) xs ++ takeWhile (/=1) xs)


day23b :: String -> Int
day23b input = runST $ do
    let cups = 10^6
    v <- initState (parseInput input) cups
    makeNMoves cups (10^7) v
    x <- MV.read v 1
    y <- MV.read v x
    return (x * y) 

makeNMoves cups n v = do
  forM_ [1..n]  (\_ -> moveb cups v)



initState :: [Int] -> Int -> ST s (MV.MVector s Int)
initState l nCups = do
    let expanded = l ++ [10..nCups]
    v <- MV.replicate (nCups + 1) 0
    forM_ (zip expanded (tail expanded)) $ \(i,n) -> do
        MV.write v i n
    MV.write v (last expanded) (head expanded)
    MV.write v 0 (head expanded)
    return v

-- helper method for testing moveb
mv input = runST $ do 
    mv <- V.thaw input 
    moveb 9 mv
    V.freeze mv

moveb :: Int -> MV.MVector s Int -> ST s ()
moveb cups v = do
    curr <- MV.read v 0
    p1 <- MV.read v curr
    p2 <- MV.read v p1
    p3 <- MV.read v p2
    notPicked <- MV.read v p3
    let dest = updateCup cups (p1,p2,p3) curr
    postDest <- MV.read v dest
    MV.write v curr notPicked
    MV.write v dest p1
    MV.write v p3 postDest
    MV.write v 0 notPicked

updateCup :: Int -> (Int,Int,Int) -> Int -> Int
updateCup maxCups (a, b, c) curr
    | p1 /= a && p1 /= b && p1 /= c = p1
    | p2 /= a && p2 /= b && p2 /= c = p2
    | p3 /= a && p3 /= b && p3 /= c = p3
    | otherwise = p4
        where p1 = if curr  == 1 then maxCups else curr  - 1
              p2 = if p1 == 1 then maxCups else p1 - 1
              p3 = if p2 == 1 then maxCups else p2 - 1
              p4 = if p3 == 1 then maxCups else p3 - 1


-- helper method for viewing linked list
_translate vec = V.scanl (\acc y -> vec!acc) 0 vec

parseInput = map digitToInt
_input = "389125467"
_input2 = "54321"
