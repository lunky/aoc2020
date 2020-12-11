module Day9
    ( 
    day9
   ,day9b
    )
    where
import Data.List (inits, tails)
    
day9 :: Int -> String -> Int 
day9 range input =  day9' range (range+1) $ parseInput input

day9' range index input 
  | validNumber curr window = day9' range (index+1) input
  | otherwise = curr
  where curr=input!!index
        window = take range $ drop (index-range) input

day9b :: Int -> String -> Int
day9b seed input = validNumber' seed (parseInput input)

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = []
combinations 1 l = map (:[]) l
combinations c l = concatMap f $ tails l
    where
            f :: [a] -> [[a]]
            f []     = []
            f (x:xs) = map (x:) $ combinations (c - 1) xs


validNumber a list = any ((==a) . sum ) $ combinations 2 list

validNumber' seed list = (\y-> maximum y + minimum y) $ concat $ filter ((==seed).sum)  $ sublists list

sublists l = filter ((>1).length) $ sublists' l
  where 
    sublists' (x:xs)
      | null xs = []
      | otherwise = inits (x:xs) ++ sublists xs

parseInput input = map (\y -> read y ::Int) $ lines input
_input = "35\n20\n15\n25\n47\n40\n62\n55\n65\n95\n102\n117\n150\n182\n127\n219\n299\n277\n309\n576\n"
