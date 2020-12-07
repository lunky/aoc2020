module Day7
    ( 
    day7
   ,day7b
   ,sumBag
    )
    where
import Data.List.Split 
import Data.List (isSuffixOf,nub)   

day7 :: String -> Int 
day7 input = (\y -> y-1) . length . nub . concat . takeWhile (/=[]) $ iterate (concatMap (findBagFor (parseInput input)))  ["shiny gold"]

day7b :: String -> Int
day7b input = sumBag (parseInput input) (1,"shiny gold")

findBagFor bags desc  = map fst $ filter (\(_,x)->any(\(a,b)->b==desc) x) bags


sumBag bags (total,desc)  = sumBag' bags (total,desc) - 1
sumBag' bags (total,desc)  = do
  let found = filter (\(a,b)->a==desc) bags
  case found of 
      [] -> total
      xs -> (+total) . (* total). sum $ map (sumBag' bags) . snd $ head xs

parseInput input = 
  map
    ((\ (x : [xs]) ->
        (unwords . init $ words x,
         map
           ((\ (z : zs) -> (read z :: Int, unwords (init zs))) . words)
           $ splitOn ", " xs))
       . splitOn " contain ")
    (filter (not.isSuffixOf "contain no other bags.") $ lines input)

_input = "light red bags contain 1 bright white bag, 2 muted yellow bags.\ndark orange bags contain 3 bright white bags, 4 muted yellow bags.\nbright white bags contain 1 shiny gold bag.\nmuted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\nshiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\ndark olive bags contain 3 faded blue bags, 4 dotted black bags.\nvibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\nfaded blue bags contain no other bags.\ndotted black bags contain no other bags.\n"
