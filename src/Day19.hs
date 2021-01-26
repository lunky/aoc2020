module Day19
    (
     day19,
     day19b
    )
    where

import Text.Parsec ((<?>))
import Text.Parsec.Expr  (Operator(..),Assoc(..),buildExpressionParser)
import Data.List.Split (splitOn)
import Control.Applicative ((<|>))
import Data.Either (isRight)
import qualified Text.ParserCombinators.Parsec as P
import qualified  Text.Parsec as Parsec
import qualified Data.IntMap as M
import Aoc (countTrue)
import Control.Monad.Identity (Identity)
import Control.Monad (guard)

type Parser = Parsec.Parsec String ()

parse :: Parser a -> String -> Either Parsec.ParseError a
parse p = P.parse p ""

day19 :: String -> Int
day19 input = day19' $ splitOn [""] $ lines input

day19' :: [[String]] -> Int
day19' [rules, patterns] = countTrue $ map matches patterns
  where
    matches s = case parse (makeParser 
                              (M.fromList $ map parseRule rules) 
                              (RuleNum 0) >> Parsec.eof) s of
                  Right _ -> True
                  Left _ -> False

day19b :: String -> Int
day19b input = day19b' $ splitOn [""] $ lines input

day19b' :: [[String]] -> Int
day19b' [rules, patterns] = countTrue $ map matches patterns
  where
    intMap = M.fromList $ map parseRule rules
    p = do 
      lhs <- Parsec.many1 $ Parsec.try $ makeParser intMap $ RuleNum 42
      rhs <- Parsec.many1 $ makeParser intMap $ RuleNum 31
      guard $ length lhs > length rhs
    matches s = isRight $ parse (p >> Parsec.eof) s 

data Rule = Literal Char 
            | And Rule Rule 
            | Or Rule Rule 
            | RuleNum Int deriving (Show, Eq, Read, Ord)

parseRule :: String -> (Int, Rule)
parseRule input = (read $ init ruleNum, rs)
  where
    Right rs = parse parseRule' $ unwords ruleBody
    (ruleNum:ruleBody) = words input
    parseRule' = buildExpressionParser table term <?> "Could not parse"
    term = (parseRuleNumber <|> parseLiteral) <* Parsec.spaces
    table = [
              [Infix parseAnd AssocLeft],
              [Infix parseOr AssocLeft]
            ]

parseRuleNumber :: Parsec.ParsecT String u Identity Rule 
parseRuleNumber = do
    l<-Parsec.many1 Parsec.digit
    return (RuleNum (read l))

parseLiteral :: Parsec.ParsecT String u Identity Rule 
parseLiteral = do
  Parsec.char '\"'
  l <- Parsec.oneOf ['a','b']
  Parsec.char '\"'
  return (Literal l)

parseAnd :: Parsec.ParsecT String u Identity (Rule -> Rule -> Rule)
parseAnd = do
  Parsec.spaces
  return And

parseOr :: Parsec.ParsecT String u Identity (Rule -> Rule -> Rule)
parseOr = do
  Parsec.char '|'
  Parsec.spaces
  return Or

makeParser :: M.IntMap Rule -> Rule -> Parser ()
makeParser intmap (And x y) = makeParser intmap x >> makeParser intmap y
makeParser intmap (Or x y) = Parsec.try (makeParser intmap x) <|> makeParser intmap y
makeParser intmap (RuleNum x) = makeParser intmap (intmap M.! x)
makeParser _ (Literal c) = () <$ Parsec.char c

_input="0: 4 1 5\n1: 2 3 | 3 2\n2: 4 4 | 5 5\n3: 4 5 | 5 4\n4: \"a\"\n5: \"b\"\n\nababbb\nbababa\nabbbab\naaabbb\naaaabbb"
_input2="42: 9 14 | 10 1\n9: 14 27 | 1 26\n10: 23 14 | 28 1\n1: \"a\"\n11: 42 31\n5: 1 14 | 15 1\n19: 14 1 | 14 14\n12: 24 14 | 19 1\n16: 15 1 | 14 14\n31: 14 17 | 1 13\n6: 14 14 | 1 14\n2: 1 24 | 14 4\n0: 8 11\n13: 14 3 | 1 12\n15: 1 | 14\n17: 14 2 | 1 7\n23: 25 1 | 22 14\n28: 16 1\n4: 1 1\n20: 14 14 | 1 15\n3: 5 14 | 16 1\n27: 1 6 | 14 18\n14: \"b\"\n21: 14 1 | 1 14\n25: 1 1 | 1 14\n22: 14 14\n8: 42\n26: 14 22 | 1 20\n18: 15 15\n7: 14 5 | 1 21\n24: 14 1\n\nabbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa\nbbabbbbaabaabba\nbabbbbaabbbbbabbbbbbaabaaabaaa\naaabbbbbbaaaabaababaabababbabaaabbababababaaa\nbbbbbbbaaaabbbbaaabbabaaa\nbbbababbbbaaaaaaaabbababaaababaabab\nababaaaaaabaaab\nababaaaaabbbaba\nbaabbaaaabbaaaababbaababb\nabbbbabbbbaaaababbbbbbaaaababb\naaaaabbaabaaaaababaa\naaaabbaaaabbaaa\naaaabbaabbaaaaaaabbbabbbaaabbaabaaa\nbabaaabbbaaabaababbaabababaaab\naabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba\n"
