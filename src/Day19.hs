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
import qualified Text.ParserCombinators.Parsec as P
import qualified  Text.Parsec as Parsec
import qualified Data.IntMap as M
import Aoc (countTrue)
import Control.Monad.Identity (Identity)

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
day19b input = 0


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
