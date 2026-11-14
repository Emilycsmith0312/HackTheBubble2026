module Main (main) where

import Bubble.Parser (parseExpr)
import Bubble.Syntax
import System.Exit

main :: IO ()
main =
  if parseExpr "\\x. x y" == Right (Lam "x" (App (Var "x") (Var "y")))
    then putStrLn "ok"
    else exitFailure
