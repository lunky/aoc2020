{-# LANGUAGE FlexibleContexts #-}
module Day19
    (
    day19
   ,day19b
   ,parse
   ,
    )
    where
import Data.List.Split
import qualified  Text.Parsec as Parsec
import Text.Parsec ((<?>))
import qualified Text.ParserCombinators.Parsec  as P
import Control.Applicative
import Control.Monad.Identity (Identity)


day19 :: String -> Int
day19 input = 0

day19b :: String -> Int
day19b input = 0

parseInput input = (\[rules,messages] -> (rules,messages) )$ splitOn "\n\n" input
_input="0: 4 1 5\n1: 2 3 | 3 2\n2: 4 4 | 5 5\n3: 4 5 | 5 4\n4: \"a\"\n5: \"b\"\n\nababbb\nbababa\nabbbab\naaabbb\naaaabbb"

ruleNum:: Parsec.Parsec String () Int
ruleNum = do
    num <- Parsec.many1 Parsec.digit
    Parsec.char ':'
    Parsec.spaces
    return (read num)

rule:: Parsec.Parsec String () [Int]
rule= do
    Parsec.spaces
    rule1 <- Parsec.many1 Parsec.digit
    Parsec.spaces
    rule2 <- Parsec.many (Parsec.many1 Parsec.digit)
    Parsec.spaces
    return (map read [rule1] ++ map read rule2)

rules:: Parsec.Parsec String () (Int, [[Int]])
rules = do
    ruleNumber <- ruleNum
    Parsec.spaces
    rulez <- Parsec.sepBy rule (Parsec.char '|' <* Parsec.spaces)
    Parsec.spaces
    return (ruleNumber, rulez)

parse :: Parsec.Stream s Identity t => Parsec.Parsec s () a -> s -> Either P.ParseError a
parse rule = P.parse rule ""
