-- | The abstract syntax tree of Bubble.
module Bubble.Syntax (Name, Expr (..)) where

type Name = String

-- | Placeholder AST; extend as the language takes shape.
data Expr
  = Var Name
  | Lam Name Expr
  | App Expr Expr
  deriving (Show, Eq)
