module Day8
    ( 
    day8
   ,day8b
    )
    where

import Data.List (nub)
import Data.Either
    
day8 :: String -> Int 
day8 input = case bootCode' (parseInput input,0,0,[]) of
               Left x -> x
               _ -> error "shouldn't get here"

day8b :: String -> Int
day8b input = 0

bootCode' (operations, pointer, acc, history) = do
  let res = case op of
              "nop" -> (operations, pointer  +1,  acc,       pointer:history)
              "acc" -> (operations, pointer  +1,  acc+arg,   pointer:history)
              "jmp" -> (operations, pointer  +arg, acc,      pointer:history)
              _ -> error "shouldn't get here"
  if foundVisited res then Left acc else bootCode' res
  where (op,arg) = operations!!pointer
        foundVisited (_,_,_,hist) = length hist /= length (nub hist)


nopJmp = map (\(i,(op,arg)) -> if op=="nop" then (i,("jmp",arg)) else (i,("nop",arg))) 
combos operations = filter (\(_,(op,_)) -> op =="nop" || op=="jmp") $ zip [0..]  operations




parseInput input = map (\y->(\[a,b]->(a,cleaned b))$words y)$ lines input
  where cleaned xs = read (filter (/='+') xs )::Int

_input="nop +0\nacc +1\njmp +4\nacc +3\njmp -3\nacc -99\nacc +1\njmp -4\nacc +6\n"
