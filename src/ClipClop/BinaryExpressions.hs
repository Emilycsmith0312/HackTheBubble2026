module Expr where

import BinarySearchTree
import Parsing
import Value
import Data.Maybe (fromMaybe)
import Control.Monad
import Text.Read (reads)
type Name = String

-- Expression data type
data Expr = Add Expr Expr     -- add two values
          | Sub Expr Expr     -- subtract two values
          | Mul Expr Expr     -- multiply two values
          | Div Expr Expr     -- divide two values
          | Mod Expr Expr     -- find the modulus of two values
          | Pow Expr Expr     -- find the power of two values
          | Abs Expr          -- find the absolute value
          | Concat Expr Expr  -- string concatination for two values
          | And Expr Expr     -- boolean AND
          | Nand Expr Expr    -- boolean NAND
          | Or Expr Expr      -- boolean OR
          | Xor Expr Expr     -- boolean XOR
          | Nor Expr Expr     -- boolean NOR
          | Imp Expr Expr     -- boolean IMPLIES
          | Eq Expr Expr      -- EQUALS
          | Not Expr          -- boolean NOT
          | ToString Expr     -- to string
          | ToInt Expr        -- to integer
          | ToFloat Expr      -- to float
          | ToBool Expr       -- to bool
          | Val Value         -- value
          | Var Name          -- variable support
  deriving Show

-- Command data type
data Command = Set Name Expr -- assign an expression to a variable name
             | Print Expr    -- evaluate an expression and print the result
  deriving Show

-- Eval is used to evaluate the expression given - recursively calls itself to evaluate the expression
eval :: Tree (Name, Value) -> -- Variable name to value tree
        Expr -> -- Expression to evaluate
        Either String Value -- Returns either an error (such as missing variables) or the result
eval vars (Val x) = Right x -- for values, just give the value directly 
eval vars (Var x) = do -- variables
                        xVal <- valueOf x vars -- find in tree
                        return xVal    
eval vars (Add x y) = do -- addition
                        xNum <- eval vars x
                        yNum <- eval vars y
                        case xNum of 
                          x -> case yNum of 
                            y -> Right (addValues x y) -- call from Value.hs
eval vars (Sub x y) = do -- subtraction
                        xNum <- eval vars x
                        yNum <- eval vars y
                        case xNum of 
                          x -> case yNum of 
                            y -> Right (subValues x y) -- call from Value.hs
eval vars (Mul x y) = do -- multiplication
                        xNum <- eval vars x
                        yNum <- eval vars y
                        case xNum of 
                          x -> case yNum of 
                            y -> Right (mulValues x y) -- call from Value.hs
eval vars (Div x y) = do --division
                        xNum <- eval vars x
                        yNum <- eval vars y
                        case xNum of 
                          x -> case yNum of 
                            y -> Right (divValues x y) -- call from Value.hs
eval vars (Mod x y) = do -- modulus
                        xNum <- eval vars x
                        yNum <- eval vars y
                        case xNum of 
                          x -> case yNum of 
                            y -> Right (modValues x y) -- call from Value.hs
eval vars (Pow x y) = do -- power
                        xNum <- eval vars x
                        yNum <- eval vars y
                        case xNum of 
                          x -> case yNum of 
                            y -> Right (powValues x y) -- call from Value.hs
eval vars (Abs x) = do -- absolute value
                        xNum <- eval vars x 
                        case xNum of 
                          x -> Right (absValues x) -- call from Value.hs
eval vars (Concat x y) = do -- string concatination
                        xString <- eval vars x   
                        yString <- eval vars y
                        case xString of 
                          x -> case yString of 
                            y -> Right (concatValues x y) -- call from Value.hs
eval vars (And x y) = do -- boolean AND
                        xString <- eval vars x   
                        yString <- eval vars y
                        case xString of 
                          x -> case yString of 
                            y -> Right (andValues x y) -- call from Value.hs
eval vars (Nand x y) = do -- boolean NAND
                        xString <- eval vars x   
                        yString <- eval vars y
                        case xString of 
                          x -> case yString of 
                            y -> Right (nandValues x y) -- call from Value.hs
eval vars (Or x y) = do -- boolean OR
                        xString <- eval vars x   
                        yString <- eval vars y
                        case xString of 
                          x -> case yString of 
                            y -> Right (orValues x y) -- call from Value.hs
eval vars (Xor x y) = do -- boolean XOR
                        xString <- eval vars x   
                        yString <- eval vars y
                        case xString of 
                          x -> case yString of 
                            y -> Right (xorValues x y) -- call from Value.hs
eval vars (Nor x y) = do -- boolean NOR
                        xString <- eval vars x   
                        yString <- eval vars y
                        case xString of 
                          x -> case yString of 
                            y -> Right (norValues x y) -- call from Value.hs
eval vars (Imp x y) = do -- boolean IMPLIES
                        xString <- eval vars x   
                        yString <- eval vars y
                        case xString of 
                          x -> case yString of 
                            y -> Right (impValues x y) -- call from Value.hs
eval vars (Eq x y) = do -- EQUALS
                        xString <- eval vars x   
                        yString <- eval vars y
                        case xString of 
                          x -> case yString of 
                            y -> Right (eqValues x y) -- call from Value.hs
eval vars (Not x) = do -- boolean NOT
                        xNum <- eval vars x 
                        case xNum of 
                          x -> Right (notValues x) -- call from Value.hs
eval vars (ToString x) = case eval vars x of 
                          Right (StrValue _ ) -> eval vars x -- if already a string return itself
                          Right (IntValue i) -> Right (StrValue (show i)) -- use show to get the string value of num types
                          Right (FloatValue f) -> Right (StrValue (show f))
                          Right (BoolValue True) -> Right (StrValue "true")
                          Right (BoolValue False) -> Right (StrValue "false")
                          _ -> Left "parse error"
eval vars (ToInt x) = case eval vars x of 
                         Right (StrValue s) -> case reads s of
                                                  [(i, "")] -> Right (IntValue i)
                                                  _ -> Left "parse error"
                         Right (FloatValue v) -> Right (IntValue (truncate v)) -- use truncate if a float
                         Right (IntValue _) -> eval vars x -- if already an int return itself
                         Right (BoolValue True) -> Right (IntValue 0)
                         Right (BoolValue False) -> Right (IntValue 1)
                         _ -> Left "parse error" 
eval vars (ToFloat x) = case eval vars x of
                          Right (StrValue s) -> case reads s of 
                                                  [(f, "")] -> Right (FloatValue f)
                                                  _ -> Left "parse error"
                          Right (FloatValue f) -> eval vars x -- if already a float return itself
                          Right (IntValue i) -> Right (FloatValue (fromIntegral i)) -- use fromIntegral if an int
                          Right (BoolValue True) -> Right (FloatValue 0.0)
                          Right (BoolValue False) -> Right (FloatValue 1.0)
                          _ -> Left "parse error"
eval vars (ToBool x) = case eval vars x of 
                          Right (BoolValue _) -> eval vars x
                          Right (IntValue 0) -> Right (BoolValue False)
                          Right (IntValue _) -> Right (BoolValue True) -- any non 0 value is true
                          Right (StrValue "false") -> Right (BoolValue False)
                          Right (StrValue "true") -> Right (BoolValue True)
                          Right (FloatValue 0.0) -> Right (BoolValue False)
                          Right (FloatValue _) -> Right (BoolValue True)
                          _ -> Left "parse error"

-- Parse the command
pCommand :: Parser Command
pCommand = do 
    space
    t <- identifier
    char '='
    e <- pExpr
    space
    return (Set t e)
  ||| do
    space
    string "print"
    space
    e <- pExpr
    space
    return (Print e)
    ||| do -- if print not specified still print the evaluated expression
      space 
      e <- pExpr
      space
      return (Print e)


pExpr :: Parser Expr
pExpr = pAbsolute ||| pNot ||| pToString ||| pToBool ||| pToNum ||| pTerm 

pTerm :: Parser Expr
pTerm = chainl1 pFactor (booleanOperators ||| concatOperator ||| modPowOperator ||| mulDivOperator ||| addSubOperator)

pFactor :: Parser Expr
pFactor = pVariable ||| pValue ||| pParenExpr

pValue :: Parser Expr
pValue = pBoolValue ||| pFloatValue ||| pIntValue ||| pStrValue

pIntValue :: Parser Expr
pIntValue = do
    space 
    i <- int
    return (Val (IntValue i))

pFloatValue :: Parser Expr
pFloatValue = do 
    space
    f <- float
    return (Val (FloatValue f))

pStrValue :: Parser Expr
pStrValue = do
    space 
    char '"'
    s <- many identifier
    char '"'
    return (Val (StrValue (pIdentifiers s)))

pBoolValue :: Parser Expr
pBoolValue = do
  space 
  string "true"
  space
  return (Val (BoolValue True))
  ||| do 
    space 
    string "false"
    space 
    return (Val (BoolValue False))

pIdentifiers :: [String] -> String
pIdentifiers [] = " " -- ensures space between words for string concatination
pIdentifiers (x:xs) = case x of 
                        "\"" ->  pIdentifiers xs
                        _ -> case xs of 
                              [] -> x ++ pIdentifiers xs
                              _ -> x ++ " " ++ pIdentifiers xs

pVariable :: Parser Expr
pVariable = do
    space
    v <- identifier
    return (Var v)

pParenExpr :: Parser Expr
pParenExpr = do
    space 
    char '('
    e <- pExpr
    char ')'
    return e

pToString :: Parser Expr
pToString = do
    space
    string "toString"
    e <- pExpr
    return (ToString e)

pToBool  :: Parser Expr
pToBool  = do
    space
    string "toBoolean"
    e <- pExpr
    return (ToBool e)

pToNum :: Parser Expr
pToNum = do 
    pToFloat 
    ||| pToInt

pToInt :: Parser Expr
pToInt = do
    space
    string "toInt"
    e <- pExpr
    return (ToInt e)

pToFloat :: Parser Expr
pToFloat = do
    space
    string "toFloat"
    e <- pExpr
    return (ToFloat e)


pAbsolute :: Parser Expr
pAbsolute = do 
    space
    string "abs"
    space
    e <- pExpr
    return (Abs e)

pNot :: Parser Expr
pNot = do 
  space 
  symbol "!"
  space 
  e <- pExpr
  return (Not e)

addSubOperator, mulDivOperator, modPowOperator, booleanOperators :: Parser (Expr -> Expr -> Expr)
addSubOperator = do
    space
    char '+'
    space
    return Add
  ||| do
    space
    char '-'
    space
    return Sub

mulDivOperator = do
    space 
    char '*'
    space
    return Mul
  ||| do
    space 
    char '/'
    space
    return Div

modPowOperator = do 
    space 
    char '%'
    space 
    return Mod
  ||| do 
    space 
    char '^'
    space 
    return Pow

concatOperator = do 
    space 
    symbol "++"
    space 
    return Concat

booleanOperators = do 
    space
    symbol "&&"
    space
    return And
    ||| do 
      space 
      symbol "!&"
      space
      return Nand
      ||| do 
        space 
        symbol "||"module Expr where

import BinarySearchTree
import Parsing
import Value
import Data.Maybe (fromMaybe)
import Control.Monad
import Text.Read (reads)
type Name = String

-- Expression data type
data Expr = Add Expr Expr     -- add two values
          | Sub Expr Expr     -- subtract two values
          | Mul Expr Expr     -- multiply two values
          | Div Expr Expr     -- divide two values
          | Mod Expr Expr     -- find the modulus of two values
          | Pow Expr Expr     -- find the power of two values
          | Abs Expr          -- find the absolute value
          | Concat Expr Expr  -- string concatination for two values
          | And Expr Expr     -- boolean AND
          | Nand Expr Expr    -- boolean NAND
          | Or Expr Expr      -- boolean OR
          | Xor Expr Expr     -- boolean XOR
          | Nor Expr Expr     -- boolean NOR
          | Imp Expr Expr     -- boolean IMPLIES
          | Eq Expr Expr      -- EQUALS
          | Not Expr          -- boolean NOT
          | ToString Expr     -- to string
          | ToInt Expr        -- to integer
          | ToFloat Expr      -- to float
          | ToBool Expr       -- to bool
          | Val Value         -- value
          | Var Name          -- variable support
  deriving Show

-- Command data type
data Command = Set Name Expr -- assign an expression to a variable name
             | Print Expr    -- evaluate an expression and print the result
  deriving Show

-- Eval is used to evaluate the expression given - recursively calls itself to evaluate the expression
eval :: Tree (Name, Value) -> -- Variable name to value tree
        Expr -> -- Expression to evaluate
        Either String Value -- Returns either an error (such as missing variables) or the result
eval vars (Val x) = Right x -- for values, just give the value directly 
eval vars (Var x) = do -- variables
                        xVal <- valueOf x vars -- find in tree
                        return xVal    
eval vars (Add x y) = do -- addition
                        xNum <- eval vars x
                        yNum <- eval vars y
                        case xNum of 
                          x -> case yNum of 
                            y -> Right (addValues x y) -- call from Value.hs
eval vars (Sub x y) = do -- subtraction
                        xNum <- eval vars x
                        yNum <- eval vars y
                        case xNum of 
                          x -> case yNum of 
                            y -> Right (subValues x y) -- call from Value.hs
eval vars (Mul x y) = do -- multiplication
                        xNum <- eval vars x
                        yNum <- eval vars y
                        case xNum of 
                          x -> case yNum of 
                            y -> Right (mulValues x y) -- call from Value.hs
eval vars (Div x y) = do --division
                        xNum <- eval vars x
                        yNum <- eval vars y
                        case xNum of 
                          x -> case yNum of 
                            y -> Right (divValues x y) -- call from Value.hs
eval vars (Mod x y) = do -- modulus
                        xNum <- eval vars x
                        yNum <- eval vars y
                        case xNum of 
                          x -> case yNum of 
                            y -> Right (modValues x y) -- call from Value.hs
eval vars (Pow x y) = do -- power
                        xNum <- eval vars x
                        yNum <- eval vars y
                        case xNum of 
                          x -> case yNum of 
                            y -> Right (powValues x y) -- call from Value.hs
eval vars (Abs x) = do -- absolute value
                        xNum <- eval vars x 
                        case xNum of 
                          x -> Right (absValues x) -- call from Value.hs
eval vars (Concat x y) = do -- string concatination
                        xString <- eval vars x   
                        yString <- eval vars y
                        case xString of 
                          x -> case yString of 
                            y -> Right (concatValues x y) -- call from Value.hs
eval vars (And x y) = do -- boolean AND
                        xString <- eval vars x   
                        yString <- eval vars y
                        case xString of 
                          x -> case yString of 
                            y -> Right (andValues x y) -- call from Value.hs
eval vars (Nand x y) = do -- boolean NAND
                        xString <- eval vars x   
                        yString <- eval vars y
                        case xString of 
                          x -> case yString of 
                            y -> Right (nandValues x y) -- call from Value.hs
eval vars (Or x y) = do -- boolean OR
                        xString <- eval vars x   
                        yString <- eval vars y
                        case xString of 
                          x -> case yString of 
                            y -> Right (orValues x y) -- call from Value.hs
eval vars (Xor x y) = do -- boolean XOR
                        xString <- eval vars x   
                        yString <- eval vars y
                        case xString of 
                          x -> case yString of 
                            y -> Right (xorValues x y) -- call from Value.hs
eval vars (Nor x y) = do -- boolean NOR
                        xString <- eval vars x   
                        yString <- eval vars y
                        case xString of 
                          x -> case yString of 
                            y -> Right (norValues x y) -- call from Value.hs
eval vars (Imp x y) = do -- boolean IMPLIES
                        xString <- eval vars x   
                        yString <- eval vars y
                        case xString of 
                          x -> case yString of 
                            y -> Right (impValues x y) -- call from Value.hs
eval vars (Eq x y) = do -- EQUALS
                        xString <- eval vars x   
                        yString <- eval vars y
                        case xString of 
                          x -> case yString of 
                            y -> Right (eqValues x y) -- call from Value.hs
eval vars (Not x) = do -- boolean NOT
                        xNum <- eval vars x 
                        case xNum of 
                          x -> Right (notValues x) -- call from Value.hs
eval vars (ToString x) = case eval vars x of 
                          Right (StrValue _ ) -> eval vars x -- if already a string return itself
                          Right (IntValue i) -> Right (StrValue (show i)) -- use show to get the string value of num types
                          Right (FloatValue f) -> Right (StrValue (show f))
                          Right (BoolValue True) -> Right (StrValue "true")
                          Right (BoolValue False) -> Right (StrValue "false")
                          _ -> Left "parse error"
eval vars (ToInt x) = case eval vars x of 
                         Right (StrValue s) -> case reads s of
                                                  [(i, "")] -> Right (IntValue i)
                                                  _ -> Left "parse error"
                         Right (FloatValue v) -> Right (IntValue (truncate v)) -- use truncate if a float
                         Right (IntValue _) -> eval vars x -- if already an int return itself
                         Right (BoolValue True) -> Right (IntValue 0)
                         Right (BoolValue False) -> Right (IntValue 1)
                         _ -> Left "parse error" 
eval vars (ToFloat x) = case eval vars x of
                          Right (StrValue s) -> case reads s of 
                                                  [(f, "")] -> Right (FloatValue f)
                                                  _ -> Left "parse error"
                          Right (FloatValue f) -> eval vars x -- if already a float return itself
                          Right (IntValue i) -> Right (FloatValue (fromIntegral i)) -- use fromIntegral if an int
                          Right (BoolValue True) -> Right (FloatValue 0.0)
                          Right (BoolValue False) -> Right (FloatValue 1.0)
                          _ -> Left "parse error"
eval vars (ToBool x) = case eval vars x of 
                          Right (BoolValue _) -> eval vars x
                          Right (IntValue 0) -> Right (BoolValue False)
                          Right (IntValue _) -> Right (BoolValue True) -- any non 0 value is true
                          Right (StrValue "false") -> Right (BoolValue False)
                          Right (StrValue "true") -> Right (BoolValue True)
                          Right (FloatValue 0.0) -> Right (BoolValue False)
                          Right (FloatValue _) -> Right (BoolValue True)
                          _ -> Left "parse error"

-- Parse the command
pCommand :: Parser Command
pCommand = do 
    space
    t <- identifier
    char '='
    e <- pExpr
    space
    return (Set t e)
  ||| do
    space
    string "print"
    space
    e <- pExpr
    space
    return (Print e)
    ||| do -- if print not specified still print the evaluated expression
      space 
      e <- pExpr
      space
      return (Print e)


pExpr :: Parser Expr
pExpr = pAbsolute ||| pNot ||| pToString ||| pToBool ||| pToNum ||| pTerm 

pTerm :: Parser Expr
pTerm = chainl1 pFactor (booleanOperators ||| concatOperator ||| modPowOperator ||| mulDivOperator ||| addSubOperator)

pFactor :: Parser Expr
pFactor = pVariable ||| pValue ||| pParenExpr

pValue :: Parser Expr
pValue = pBoolValue ||| pFloatValue ||| pIntValue ||| pStrValue

pIntValue :: Parser Expr
pIntValue = do
    space 
    i <- int
    return (Val (IntValue i))

pFloatValue :: Parser Expr
pFloatValue = do 
    space
    f <- float
    return (Val (FloatValue f))

pStrValue :: Parser Expr
pStrValue = do
    space 
    char '"'
    s <- many identifier
    char '"'
    return (Val (StrValue (pIdentifiers s)))

pBoolValue :: Parser Expr
pBoolValue = do
  space 
  string "true"
  space
  return (Val (BoolValue True))
  ||| do 
    space 
    string "false"
    space 
    return (Val (BoolValue False))

pIdentifiers :: [String] -> String
pIdentifiers [] = " " -- ensures space between words for string concatination
pIdentifiers (x:xs) = case x of 
                        "\"" ->  pIdentifiers xs
                        _ -> case xs of 
                              [] -> x ++ pIdentifiers xs
                              _ -> x ++ " " ++ pIdentifiers xs

pVariable :: Parser Expr
pVariable = do
    space
    v <- identifier
    return (Var v)

pParenExpr :: Parser Expr
pParenExpr = do
    space 
    char '('
    e <- pExpr
    char ')'
    return e

pToString :: Parser Expr
pToString = do
    space
    string "toString"
    e <- pExpr
    return (ToString e)

pToBool  :: Parser Expr
pToBool  = do
    space
    string "toBoolean"
    e <- pExpr
    return (ToBool e)

pToNum :: Parser Expr
pToNum = do 
    pToFloat 
    ||| pToInt

pToInt :: Parser Expr
pToInt = do
    space
    string "toInt"
    e <- pExpr
    return (ToInt e)

pToFloat :: Parser Expr
pToFloat = do
    space
    string "toFloat"
    e <- pExpr
    return (ToFloat e)


pAbsolute :: Parser Expr
pAbsolute = do 
    space
    string "abs"
    space
    e <- pExpr
    return (Abs e)

pNot :: Parser Expr
pNot = do 
  space 
  symbol "!"
  space 
  e <- pExpr
  return (Not e)

addSubOperator, mulDivOperator, modPowOperator, booleanOperators :: Parser (Expr -> Expr -> Expr)
addSubOperator = do
    space
    char '+'
    space
    return Add
  ||| do
    space
    char '-'
    space
    return Sub

mulDivOperator = do
    space 
    char '*'
    space
    return Mul
  ||| do
    space 
    char '/'
    space
    return Div

modPowOperator = do 
    space 
    char '%'
    space 
    return Mod
  ||| do 
    space 
    char '^'module Expr where

import BinarySearchTree
import Parsing
import Value
import Data.Maybe (fromMaybe)
import Control.Monad
import Text.Read (reads)
type Name = String

-- Expression data type
data Expr = Add Expr Expr     -- add two values
          | Sub Expr Expr     -- subtract two values
          | Mul Expr Expr     -- multiply two values
          | Div Expr Expr     -- divide two values
          | Mod Expr Expr     -- find the modulus of two values
          | Pow Expr Expr     -- find the power of two values
          | Abs Expr          -- find the absolute value
          | Concat Expr Expr  -- string concatination for two values
          | And Expr Expr     -- boolean AND
          | Nand Expr Expr    -- boolean NAND
          | Or Expr Expr      -- boolean OR
          | Xor Expr Expr     -- boolean XOR
          | Nor Expr Expr     -- boolean NOR
          | Imp Expr Expr     -- boolean IMPLIES
          | Eq Expr Expr      -- EQUALS
          | Not Expr          -- boolean NOT
          | ToString Expr     -- to string
          | ToInt Expr        -- to integer
          | ToFloat Expr      -- to float
          | ToBool Expr       -- to bool
          | Val Value         -- value
          | Var Name          -- variable support
  deriving Show

-- Command data type
data Command = Set Name Expr -- assign an expression to a variable name
             | Print Expr    -- evaluate an expression and print the result
  deriving Show

-- Eval is used to evaluate the expression given - recursively calls itself to evaluate the expression
eval :: Tree (Name, Value) -> -- Variable name to value tree
        Expr -> -- Expression to evaluate
        Either String Value -- Returns either an error (such as missing variables) or the result
eval vars (Val x) = Right x -- for values, just give the value directly 
eval vars (Var x) = do -- variables
                        xVal <- valueOf x vars -- find in tree
                        return xVal    
eval vars (Add x y) = do -- addition
                        xNum <- eval vars x
                        yNum <- eval vars y
                        case xNum of 
                          x -> case yNum of 
                            y -> Right (addValues x y) -- call from Value.hs
eval vars (Sub x y) = do -- subtraction
                        xNum <- eval vars x
                        yNum <- eval vars y
                        case xNum of 
                          x -> case yNum of 
                            y -> Right (subValues x y) -- call from Value.hs
eval vars (Mul x y) = do -- multiplication
                        xNum <- eval vars x
                        yNum <- eval vars y
                        case xNum of 
                          x -> case yNum of 
                            y -> Right (mulValues x y) -- call from Value.hs
eval vars (Div x y) = do --division
                        xNum <- eval vars x
                        yNum <- eval vars y
                        case xNum of 
                          x -> case yNum of 
                            y -> Right (divValues x y) -- call from Value.hs
eval vars (Mod x y) = do -- modulus
                        xNum <- eval vars x
                        yNum <- eval vars y
                        case xNum of 
                          x -> case yNum of 
                            y -> Right (modValues x y) -- call from Value.hs
eval vars (Pow x y) = do -- power
                        xNum <- eval vars x
                        yNum <- eval vars y
                        case xNum of 
                          x -> case yNum of 
                            y -> Right (powValues x y) -- call from Value.hs
eval vars (Abs x) = do -- absolute value
                        xNum <- eval vars x 
                        case xNum of 
                          x -> Right (absValues x) -- call from Value.hs
eval vars (Concat x y) = do -- string concatination
                        xString <- eval vars x   
                        yString <- eval vars y
                        case xString of 
                          x -> case yString of 
                            y -> Right (concatValues x y) -- call from Value.hs
eval vars (And x y) = do -- boolean AND
                        xString <- eval vars x   
                        yString <- eval vars y
                        case xString of 
                          x -> case yString of 
                            y -> Right (andValues x y) -- call from Value.hs
eval vars (Nand x y) = do -- boolean NAND
                        xString <- eval vars x   
                        yString <- eval vars y
                        case xString of 
                          x -> case yString of 
                            y -> Right (nandValues x y) -- call from Value.hs
eval vars (Or x y) = do -- boolean OR
                        xString <- eval vars x   
                        yString <- eval vars y
                        case xString of 
                          x -> case yString of 
                            y -> Right (orValues x y) -- call from Value.hs
eval vars (Xor x y) = do -- boolean XOR
                        xString <- eval vars x   
                        yString <- eval vars y
                        case xString of 
                          x -> case yString of 
                            y -> Right (xorValues x y) -- call from Value.hs
eval vars (Nor x y) = do -- boolean NOR
                        xString <- eval vars x   
                        yString <- eval vars y
                        case xString of 
                          x -> case yString of 
                            y -> Right (norValues x y) -- call from Value.hs
eval vars (Imp x y) = do -- boolean IMPLIES
                        xString <- eval vars x   
                        yString <- eval vars y
                        case xString of 
                          x -> case yString of 
                            y -> Right (impValues x y) -- call from Value.hs
eval vars (Eq x y) = do -- EQUALS
                        xString <- eval vars x   
                        yString <- eval vars y
                        case xString of 
                          x -> case yString of 
                            y -> Right (eqValues x y) -- call from Value.hs
eval vars (Not x) = do -- boolean NOT
                        xNum <- eval vars x 
                        case xNum of 
                          x -> Right (notValues x) -- call from Value.hs
eval vars (ToString x) = case eval vars x of 
                          Right (StrValue _ ) -> eval vars x -- if already a string return itself
                          Right (IntValue i) -> Right (StrValue (show i)) -- use show to get the string value of num types
                          Right (FloatValue f) -> Right (StrValue (show f))
                          Right (BoolValue True) -> Right (StrValue "true")
                          Right (BoolValue False) -> Right (StrValue "false")
                          _ -> Left "parse error"
eval vars (ToInt x) = case eval vars x of 
                         Right (StrValue s) -> case reads s of
                                                  [(i, "")] -> Right (IntValue i)
                                                  _ -> Left "parse error"
                         Right (FloatValue v) -> Right (IntValue (truncate v)) -- use truncate if a float
                         Right (IntValue _) -> eval vars x -- if already an int return itself
                         Right (BoolValue True) -> Right (IntValue 0)
                         Right (BoolValue False) -> Right (IntValue 1)
                         _ -> Left "parse error" 
eval vars (ToFloat x) = case eval vars x of
                          Right (StrValue s) -> case reads s of 
                                                  [(f, "")] -> Right (FloatValue f)
                                                  _ -> Left "parse error"
                          Right (FloatValue f) -> eval vars x -- if already a float return itself
                          Right (IntValue i) -> Right (FloatValue (fromIntegral i)) -- use fromIntegral if an int
                          Right (BoolValue True) -> Right (FloatValue 0.0)
                          Right (BoolValue False) -> Right (FloatValue 1.0)
                          _ -> Left "parse error"
eval vars (ToBool x) = case eval vars x of 
                          Right (BoolValue _) -> eval vars x
                          Right (IntValue 0) -> Right (BoolValue False)
                          Right (IntValue _) -> Right (BoolValue True) -- any non 0 value is true
                          Right (StrValue "false") -> Right (BoolValue False)
                          Right (StrValue "true") -> Right (BoolValue True)
                          Right (FloatValue 0.0) -> Right (BoolValue False)
                          Right (FloatValue _) -> Right (BoolValue True)
                          _ -> Left "parse error"

-- Parse the command
pCommand :: Parser Command
pCommand = do 
    space
    t <- identifier
    char '='
    e <- pExpr
    space
    return (Set t e)
  ||| do
    space
    string "print"
    space
    e <- pExpr
    space
    return (Print e)
    ||| do -- if print not specified still print the evaluated expression
      space 
      e <- pExpr
      space
      return (Print e)


pExpr :: Parser Expr
pExpr = pAbsolute ||| pNot ||| pToString ||| pToBool ||| pToNum ||| pTerm 

pTerm :: Parser Expr
pTerm = chainl1 pFactor (booleanOperators ||| concatOperator ||| modPowOperator ||| mulDivOperator ||| addSubOperator)

pFactor :: Parser Expr
pFactor = pVariable ||| pValue ||| pParenExpr

pValue :: Parser Expr
pValue = pBoolValue ||| pFloatValue ||| pIntValue ||| pStrValue

pIntValue :: Parser Expr
pIntValue = do
    space 
    i <- int
    return (Val (IntValue i))

pFloatValue :: Parser Expr
pFloatValue = do 
    space
    f <- float
    return (Val (FloatValue f))

pStrValue :: Parser Expr
pStrValue = do
    space 
    char '"'
    s <- many identifier
    char '"'
    return (Val (StrValue (pIdentifiers s)))

pBoolValue :: Parser Expr
pBoolValue = do
  space 
  string "true"
  space
  return (Val (BoolValue True))
  ||| do 
    space 
    string "false"
    space 
    return (Val (BoolValue False))

pIdentifiers :: [String] -> String
pIdentifiers [] = " " -- ensures space between words for string concatination
pIdentifiers (x:xs) = case x of 
                        "\"" ->  pIdentifiers xs
                        _ -> case xs of 
                              [] -> x ++ pIdentifiers xs
                              _ -> x ++ " " ++ pIdentifiers xs

pVariable :: Parser Expr
pVariable = do
    space
    v <- identifier
    return (Var v)

pParenExpr :: Parser Expr
pParenExpr = do
    space 
    char '('
    e <- pExpr
    char ')'
    return e

pToString :: Parser Expr
pToString = do
    space
    string "toString"
    e <- pExpr
    return (ToString e)

pToBool  :: Parser Expr
pToBool  = do
    space
    string "toBoolean"
    e <- pExpr
    return (ToBool e)

pToNum :: Parser Expr
pToNum = do 
    pToFloat 
    ||| pToInt

pToInt :: Parser Expr
pToInt = do
    space
    string "toInt"
    e <- pExpr
    return (ToInt e)

pToFloat :: Parser Expr
pToFloat = do
    space
    string "toFloat"
    e <- pExpr
    return (ToFloat e)


pAbsolute :: Parser Expr
pAbsolute = do 
    space
    string "abs"
    space
    e <- pExpr
    return (Abs e)

pNot :: Parser Expr
pNot = do 
  space 
  symbol "!"
  space 
  e <- pExpr
  return (Not e)

addSubOperator, mulDivOperator, modPowOperator, booleanOperators :: Parser (Expr -> Expr -> Expr)
addSubOperator = do
    space
    char '+'
    space
    return Add
  ||| do
    space
    char '-'
    space
    return Sub

mulDivOperator = do
    space 
    char '*'
    space
    return Mul
  ||| do
    space 
    char '/'
    space
    return Div

modPowOperator = do 
    space 
    char '%'
    space 
    return Mod
  ||| do 
    space 
    char '^'
    space 
    return Pow

concatOperator = do 
    space 
    symbol "++"
    space 
    return Concat

booleanOperators = do 
    space
    symbol "&&"
    space
    return And
    ||| do 
      space 
      symbol "!&"
      space
      return Nand
      ||| do 
        space 
        symbol "||"
        space
        return Or
        ||| do 
          space 
          symbol "!|"
          space
          return Nor
          ||| do 
            space 
            symbol "x|"
            space
            return Xor
            ||| do 
              space
              symbol "->"
              space
              return Imp
              ||| do
                space 
                symbol "=="
                space
                return Eq
    space 
    return Pow

concatOperator = do 
    space 
    symbol "++"
    space 
    return Concat

booleanOperators = do 
    space
    symbol "&&"
    space
    return And
    ||| do 
      space 
      symbol "!&"
      space
      return Nand
      ||| do 
        space 
        symbol "||"
        space
        return Or
        ||| do 
          space 
          symbol "!|"
          space
          return Nor
          ||| do 
            space 
            symbol "x|"
            space
            return Xor
            ||| do 
              space
              symbol "->"
              space
              return Imp
              ||| do
                space 
                symbol "=="
                space
                return Eq
        space
        return Or
        ||| do 
          space 
          symbol "!|"
          space
          return Nor
          ||| do 
            space 
            symbol "x|"
            space
            return Xor
            ||| do 
              space
              symbol "->"
              space
              return Imp
              ||| do
                space 
                symbol "=="
                space
                return Eq