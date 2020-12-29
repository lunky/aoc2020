module Day23
    ( 
    day23
   ,day23b
   ,move
    )
    where
import Data.Char (digitToInt)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

day23 :: String -> Int 
day23 input = 0 

day23b :: String -> Int
day23b input = 0

nextDestCup x xs = case elemIndex x xs of
                        Just y -> y
                        Nothing -> nextDestCup nextIndex xs
  where nextIndex = case x of 
                      0 -> 9
                      y -> y - 1


move (offset,set) =  (offset+1,take destCup (intermediate ++ intermediate)
                             ++ pickup  
                             ++ take (9-(destCup+3)) (drop destCup intermediate++intermediate))
  where destCup = nextDestCup ((intermediate !! offset)-1) intermediate
        pickup = take 3 $ drop (offset + 1)  (set ++ set)
        intermediate = take (offset +1) set 
                        ++ take (9-(offset + 1 + 3))  
                            (drop (offset + 1 + 3) (set ++ set))

parseInput = map digitToInt
_input = "389125467"
