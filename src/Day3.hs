module Day3
    (  day3
      ,day3'
      ,day3b
    ) where

day3 :: String -> Int
day3 input = day3' (words input) 0 3 1 


day3b :: String -> Int
day3b input =   day3' (words input) 0 1 1 
              * day3' (words input) 0 3 1 
              * day3' (words input) 0 5 1 
              * day3' (words input) 0 7 1 
              * day3' (words input) 0 1 2


day3':: [String] -> Int -> Int -> Int -> Int
day3' [] _ _ _ = 0
day3' lst@(y : _) x dx dy = n + day3' (drop dy lst) x' dx dy
  where
      n = if y !! x == '#' then 1 else 0
      x' = mod (x + dx) (length y)

_input = "..##.......\n#...#...#..\n.#....#..#.\n..#.#...#.#\n.#...##..#.\n..#.##.....\n.#.#.#....#\n.#........#\n#.##...#...\n#...##....#\n.#..#...#.#\n"
