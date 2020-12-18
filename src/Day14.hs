module Day14
    ( 
    day14
   ,day14b
   ,intToBinary
   ,binaryToInt
   ,setMem
    )
    where
import Data.Char (intToDigit,digitToInt)
import Data.List (isPrefixOf)
import Data.List.Split (splitOn)
import Data.Map ()
import qualified Data.Map as Map
import Data.Vector ()
import qualified Data.Vector as V
import Control.Monad

day14 :: String -> Int 
day14 input = Map.foldr (\y acc -> binaryToInt y + acc ) 0 $ fst $ foldr readInstructions (Map.empty,"") $ reverse $ parseInput input

day14b :: String -> Int
day14b input = sum $ map ((\y->read y::Int) . snd) $ Map.toList $ fst $ foldr readInstructionsb (Map.empty,"") $ reverse $ parseInput input

readInstructions (instruction,val) (curr, mask) 
  | instruction == "mask" = (curr, val)
  | "mem" `isPrefixOf` instruction = (Map.insert instruction (setMem mask (intToBinary (read val::Int))) curr, mask)
  | otherwise = error ("invalid instruction " ++ instruction)

readInstructionsb (instruction,val) (curr, mask) 
  | instruction == "mask" = (curr, val)
  | "mem" `isPrefixOf` instruction = (foldr (`Map.insert` val) curr $ floatingVariations $ setMemb mask (intToBinary key),mask)
  | otherwise = error ("invalid instruction " ++ instruction)
    where key = (\y->read y::Int)$ tail $ init $ dropWhile (/='[') instruction


floatingVariations = \z->map (\a->V.toList$ V.fromList z V.// a) $ variations $ (map fst . filter (\(_,x)->x=='X') . zip [0..] ) z 
  where variations a = map (zip a) $ replicateM (length a) ['0','1'] 

intToBinary i = pad $ convert i ""
  where pad i = reverse $ take 36 $ reverse i ++ replicate 36 '0' 
        convert b ret 
          | b==0 = ret
          | otherwise = convert quotient (remainder:ret)
          where quotient = b `div` 2
                remainder = intToDigit (b `mod` 2)

binaryToInt b = foldr (\(i,x) acc -> digitToInt x * (2^i) + acc) 0 $ zip [0..] $ reverse b

setMem = zipWith (\m v -> if m == 'X' then v else m) 
setMemb = zipWith (\m v -> if m == '0' then v else if m=='1' then m else 'X') 

parseInput input = map ((\ [a, b] -> (a, b)) . splitOn " = ") (lines input)

_input = "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X\nmem[8] = 11\nmem[7] = 101\nmem[8] = 0"
_input2 = "000000000000000000000000000000X1001X"
_input3 = "mask = 000000000000000000000000000000X1001X\nmem[42] = 100\nmask = 00000000000000000000000000000000X0XX\nmem[26] = 1"
