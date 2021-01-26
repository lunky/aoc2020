module Day18b
    (
    day18b
   ,calculateb
    )
    where

import Control.Applicative
import Aoc (   eos
              ,runParser
              ,spaces
              ,digit
              ,many1
              ,char
              ,between
              ,Parser)

day18b :: String -> Int
day18b input =  case fmap sum $ mapM calculateb $ lines input of
                  Just x -> fromIntegral x
                  _ -> error "error, something didn't compute"

data Expr = Add Expr Expr
          | Mul Expr Expr
          | Lit Integer deriving ( Eq, Show )

eval :: Expr -> Integer
eval (Lit a) = a
eval (Add l r) = eval l + eval r
eval (Mul l r) = eval l * eval r

spaceChar :: Char -> Parser String Char
spaceChar c = between spaces spaces (char c)

literal :: Parser String Expr
literal = Lit . read <$> (spaces *> many1 digit <* spaces)

add :: Parser String Expr
add = Add <$> factor <*> (spaceChar '+' *> term)

mul :: Parser String Expr
mul = Mul <$> term <*> (spaceChar '*' *> expr)

parens = between (spaceChar ')') (spaceChar '(') expr

factor :: Parser String Expr
factor = literal <|> parens

term :: Parser String Expr
term = add <|> factor

expr :: Parser String Expr
expr = mul <|> term

parse :: String -> Maybe Expr
parse = fmap fst . runParser (expr <* eos)

calculateb :: String -> Maybe Integer
calculateb str = (fmap eval . parse) $ reverse str

