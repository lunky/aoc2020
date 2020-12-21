module Day16
    ( 
    day16
   ,day16b
    )
    where
import Data.List.Split (splitOn)
import Data.List (isPrefixOf,transpose)
    
data Rule = Rule { name :: String, range1 :: (Int,Int), range2 :: (Int,Int) } deriving (Show,Eq)

day16 :: String -> Int 
day16 input = sum $ day16' input

day16' input = match rules nearbyNeighbors
  where rules = (\(a,b,c)->a) parsedInput
        nearbyNeighbors = (\(a,b,c)->c) parsedInput
        parsedInput = parseInput input 
        match rules nearby = foldr match' (concat nearby) rules
        match' (Rule name range1 range2) nearby = match'' range1 $ match'' range2 nearby
        match'' (lower,upper) = filter (\y -> y>upper || y<lower)


day16b :: String -> Int
day16b input = product $ map (\y-> myTicket !! fst y)
                $ filter (\(a,Rule name _ _ )-> "departure" `isPrefixOf` name)
                $ solve rules columns []
  where parsedInput = filter ( not . any (`elem` day16' input)) neighbors
        (rules, myTicket, neighbors) = parseInput input
        columns = zip [0..] (transpose parsedInput)
        solve rules (c:cs) result
          | (c:cs) == [c] = (fst c,head rules):result
          | otherwise = do
              let cols = filter (\r->matchb' r (snd c)) rules
              if length cols == 1 then 
                  solve (filter (/= head cols) rules) cs ((fst c,head cols):result) 
              else 
                  solve rules (cs++[c]) result
          where
            matchb' (Rule name range1 range2) 
              = all (\y -> matchb'' range1 y || matchb'' range2 y)
                where
                  matchb'' (lower,upper) el = el>=lower && el<=upper


parseInput input =  (\[a,b,c]->(parseRules a,parseTicket b,parseTickets c)) $ splitOn "\n\n" input
 where parseTicket b = map (\x->read x::Int) $ splitOn "," $ head $ tail $ lines b
       parseTickets c = map (map (\x->read x::Int) . splitOn ",") $ tail $ lines c
       parseRules a = map ((\[a,b]->(\(name,(range1,range2))->(Rule {name=name, range1=range1, range2=range2}))
                             (a,
                              (\[x,y]->(x,y)) 
                                $ map ((\[i,j]->(i,j)). map (\x->read x::Int). splitOn "-") 
                                       $ splitOn " or " b))
                             . splitOn ": ")
                            (lines a)

_input = "class: 1-3 or 5-7\nrow: 6-11 or 33-44\nseat: 13-40 or 45-50\n\nyour ticket:\n7,1,14\n\nnearby tickets:\n7,3,47\n40,4,50\n55,2,20\n38,6,12"
_input2 = "departure class: 0-1 or 4-19\ndeparture row: 0-5 or 8-19\ndeparture seat: 0-13 or 16-19\n\nyour ticket:\n11,12,13\n\nnearby tickets:\n3,9,18\n15,1,5\n5,14,9"
