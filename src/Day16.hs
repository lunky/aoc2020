module Day16
    ( 
    day16
   ,day16b
    )
    where
import Data.List.Split (splitOn)
    
day16 :: String -> Int 
day16 input = sum $ match rules nearbyNeighbors
  where rules = (\(a,b,c)->a) parsedInput
        nearbyNeighbors = (\(a,b,c)->c) parsedInput
        parsedInput = parseInput input 

match rules nearby = foldr match' (concat nearby) rules
match' (Rule name range1 range2) nearby = match'' range1 $ match'' range2 nearby
match'' (upper,lower) = filter (\y -> y>lower || y<upper)


day16b :: String -> Int
day16b input = 0

data Rule = Rule { name :: String, range1 :: (Int,Int), range2 :: (Int,Int) } deriving (Show)

parseRules a = map ((\[a,b]->(\(name,(range1,range2))->(Rule {name=name, range1=range1, range2=range2}))
                             (a,
                              (\[x,y]->(x,y)) 
                                $ map ((\[i,j]->(i,j)). map (\x->read x::Int). splitOn "-") 
                                       $ splitOn " or " b))
                             . splitOn ": ")
                            (lines a)

parseTicket b = map (\x->read x::Int) $ splitOn "," $ head $ tail $ lines b

parseTickets c = map (map (\x->read x::Int) . splitOn ",") $ tail $ lines c

parseInput input =  (\[a,b,c]->(parseRules a,parseTicket b,parseTickets c)) $ splitOn "\n\n" input

_input = "class: 1-3 or 5-7\nrow: 6-11 or 33-44\nseat: 13-40 or 45-50\n\nyour ticket:\n7,1,14\n\nnearby tickets:\n7,3,47\n40,4,50\n55,2,20\n38,6,12"

