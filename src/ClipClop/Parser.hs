-- | Parsec-based parser for ClipClop.
module ClipClop.Parser (parseExpr) where

import ClipClop.Syntax
import Text.Parsec
import Text.Parsec.String (Parser)

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (spaces *> expr <* eof) "<repl>"

expr :: Parser Expr
expr = foldl1 App <$> many1 (term <* spaces)

term :: Parser Expr
term = lambda <|> var <|> between (char '(' <* spaces) (char ')') expr

lambda :: Parser Expr
lambda = Lam <$> (char '\\' *> spaces *> ident <* spaces <* char '.' <* spaces) <*> expr

var :: Parser Expr
var = Var <$> ident

ident :: Parser Name
ident = many1 letter
