module ClipClop.BinaryExpressions where

import ClipClop.Value
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Parsec hiding (Empty)
import Text.Parsec.String (Parser)

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
data Command = Set Type Name Expr -- assign an expression to a variable name: the {type} {name} says {expr}
             | Print Expr         -- evaluate an expression and print the result
  deriving Show

-- Variable types
data Type = TInt     -- SingularHorse
          | TDouble  -- PairOfHorses
          | TChar    -- Shetland
          | TString  -- SuffolkPunch
          | TBool    -- Appaloosa
          | TUnit    -- Unitcorn
  deriving (Show, Eq)

-- | Check a value has the declared type, giving a type error if not.
checkType :: Type -> Value -> Either String Value
checkType t v
  | typeOf v == Just t = Right v
  | otherwise          = Left ("type mismatch: expected " ++ typeName t ++ " but got " ++ maybe "an error" typeName (typeOf v))

typeOf :: Value -> Maybe Type
typeOf (IntValue _)   = Just TInt
typeOf (FloatValue _) = Just TDouble
typeOf (CharValue _)  = Just TChar
typeOf (StrValue _)   = Just TString
typeOf (BoolValue _)  = Just TBool
typeOf UnitValue      = Just TUnit
typeOf (ErrValue _)   = Nothing

-- | The Clip Clop name of each type.
typeNames :: [(Type, String)]
typeNames = [ (TInt, "SingularHorse"), (TDouble, "PairOfHorses"), (TChar, "Shetland")
            , (TString, "SuffolkPunch"), (TBool, "Appaloosa"), (TUnit, "Unitcorn") ]

typeName :: Type -> String
typeName t = maybe (show t) id (lookup t typeNames)

-- Eval is used to evaluate the expression given - recursively calls itself to evaluate the expression
eval :: Map Name Value -> -- Variable name to value map
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
                          Right (IntValue i) -> Right (StrValue (show (IntValue i))) -- binary cl0p/cl1p form
                          Right (FloatValue f) -> Right (StrValue (show (FloatValue f)))
                          Right (BoolValue True) -> Right (StrValue kwTrue)
                          Right (BoolValue False) -> Right (StrValue kwFalse)
                          Right (CharValue c) -> Right (StrValue [c])
                          _ -> Left "parse error"
eval vars (ToInt x) = case eval vars x of 
                         Right (StrValue s) -> case parseNumber s of -- string must hold a cl0p/cl1p number
                                                  Just (FloatValue v) -> Right (IntValue (truncate v))
                                                  Just v -> Right v
                                                  Nothing -> Left "parse error"
                         Right (FloatValue v) -> Right (IntValue (truncate v)) -- use truncate if a float
                         Right (IntValue _) -> eval vars x -- if already an int return itself
                         Right (BoolValue True) -> Right (IntValue 1)
                         Right (BoolValue False) -> Right (IntValue 0)
                         _ -> Left "parse error" 
eval vars (ToFloat x) = case eval vars x of
                          Right (StrValue s) -> case parseNumber s of -- string must hold a cl0p/cl1p number
                                                  Just (IntValue i) -> Right (FloatValue (fromIntegral i))
                                                  Just v -> Right v
                                                  Nothing -> Left "parse error"
                          Right (FloatValue f) -> eval vars x -- if already a float return itself
                          Right (IntValue i) -> Right (FloatValue (fromIntegral i)) -- use fromIntegral if an int
                          Right (BoolValue True) -> Right (FloatValue 1.0)
                          Right (BoolValue False) -> Right (FloatValue 0.0)
                          _ -> Left "parse error"
eval vars (ToBool x) = case eval vars x of 
                          Right (BoolValue _) -> eval vars x
                          Right (IntValue 0) -> Right (BoolValue False)
                          Right (IntValue _) -> Right (BoolValue True) -- any non 0 value is true
                          Right (StrValue s) | s == kwFalse -> Right (BoolValue False)
                          Right (StrValue s) | s == kwTrue -> Right (BoolValue True)
                          Right (FloatValue 0.0) -> Right (BoolValue False)
                          Right (FloatValue _) -> Right (BoolValue True)
                          _ -> Left "parse error"

-- | Look a variable up in the environment.
valueOf :: Name -> Map Name Value -> Either String Value
valueOf x vars = maybe (Left ("variable not defined: " ++ x)) Right (Map.lookup x vars)

-- PARSING (Parsec) --

-- | Parse a complete line of input as a command.
parseCommand :: String -> Either ParseError Command
parseCommand = parse (pCommand <* eof) "<repl>"

-- | Run a parser, then skip any trailing whitespace.
lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

-- | Parse an operator/punctuation token (backtracks if it only partly matches).
symbol :: String -> Parser String
symbol = try . lexeme . string

-- | Parse a keyword, making sure it is not just the prefix of an identifier.
keyword :: String -> Parser ()
keyword s = try (lexeme (string s *> notFollowedBy alphaNum))

-- KEYWORDS --
-- Every keyword is defined once here; the parsers and 'reserved' both use these.

kwTrue, kwFalse, kwPrint, kwAbs, kwToString, kwToFloat, kwToInt, kwToBool :: String
kwTrue     = "neighneigh"     -- true
kwFalse    = "neigh"          -- false
kwPrint    = "speakupgirl"    -- print
kwAbs      = "geld"           -- abs
kwToString = "toSuffolkPunch" -- toString
kwToFloat  = "maretosis"      -- toFloat
kwToInt    = "amalgamare"     -- toInt
kwToBool   = "addSpots"       -- toBoolean
kwThe, kwSays :: String
kwThe      = "the"            -- start of a variable declaration
kwSays     = "says"           -- between the variable name and its value

-- | Words that cannot be used as variable names.
reserved :: [String]
reserved = [kwTrue, kwFalse, kwPrint, kwAbs, kwToString, kwToFloat, kwToInt, kwToBool, kwThe, kwSays]
        ++ ["duplicateHorse"] -- reserved for future use

identifier :: Parser Name
identifier = try $ do
    name <- lexeme ((:) <$> lower <*> many alphaNum)
    if name `elem` reserved then unexpected ("keyword " ++ show name) else return name

-- Parse the command
pCommand :: Parser Command
pCommand = spaces *> (pSet <|> pPrint)

-- define variable or function
-- the {type} {var name} says {expression}
pSet :: Parser Command
pSet = do
    keyword kwThe
    t <- pType
    name <- identifier
    keyword kwSays
    Set t name <$> pExpr

pType :: Parser Type
pType = choice [t <$ keyword n | (t, n) <- typeNames] <?> "a type"

-- print 
pPrint :: Parser Command
pPrint = optional (keyword kwPrint) *> (Print <$> pExpr)

pExpr :: Parser Expr
pExpr = pAbsolute <|> pNot <|> pToString <|> pToBool <|> pToNum <|> pTerm

pTerm :: Parser Expr
pTerm = chainl1 pFactor (booleanOperators <|> concatOperator <|> modPowOperator <|> mulDivOperator <|> addSubOperator)

pFactor :: Parser Expr
pFactor = pValue <|> pVariable <|> pParenExpr

pValue :: Parser Expr
pValue = pBoolValue <|> pNumValue <|> pStrValue <|> pCharValue <|> pUnitValue

-- | A character literal: 'a'
pCharValue :: Parser Expr
pCharValue = lexeme $ Val . CharValue <$> between (char '\'') (char '\'') (noneOf "'")

-- | The unit value: ()
pUnitValue :: Parser Expr
pUnitValue = Val UnitValue <$ try (symbol "(" *> symbol ")")

-- NUMBERS --
-- Binary, one word per digit: cl0p = 0, cl1p = 1, written with no spaces.
--   cl1pcl0pcl1p = 5     -cl1pcl0p = -2     cl1pcl0p.cl1p = 2.5

pBit :: Parser Bool
pBit = (False <$ try (string "cl0p")) <|> (True <$ try (string "cl1p"))

-- | An integer, or a float if there is a binary point.
pNumber :: Parser Value
pNumber = try $ do
    neg <- option False (True <$ char '-')
    whole <- many1 pBit
    frac <- optionMaybe (char '.' *> many1 pBit)
    notFollowedBy alphaNum -- so e.g. "cl1pfoo" isn't a number followed by junk
    let sgn :: Num a => a -> a
        sgn = if neg then negate else id
    return $ case frac of
        Nothing -> IntValue (sgn (fromInteger (bitsToInt whole)))
        Just fs -> FloatValue (sgn (fromRational (fromInteger (bitsToInt whole) + bitsToFrac fs)))

pNumValue :: Parser Expr
pNumValue = Val <$> lexeme pNumber

-- | Read a whole string as a number (used by the conversion keywords).
parseNumber :: String -> Maybe Value
parseNumber s = either (const Nothing) Just (parse (spaces *> pNumber <* spaces <* eof) "" s)

pStrValue :: Parser Expr
pStrValue = lexeme $ do
    _ <- char '"'
    s <- many (noneOf "\"")
    _ <- char '"'
    return (Val (StrValue s))

pBoolValue :: Parser Expr
pBoolValue = (Val (BoolValue True) <$ keyword kwTrue)
         <|> (Val (BoolValue False) <$ keyword kwFalse)

pVariable :: Parser Expr
pVariable = Var <$> identifier

pParenExpr :: Parser Expr
pParenExpr = between (symbol "(") (symbol ")") pExpr

pToString, pToBool, pToNum, pToInt, pToFloat, pAbsolute, pNot :: Parser Expr
pToString = ToString <$> (keyword kwToString *> pExpr)
pToBool   = ToBool   <$> (keyword kwToBool *> pExpr)
pToNum    = pToFloat <|> pToInt
pToInt    = ToInt    <$> (keyword kwToInt *> pExpr)
pToFloat  = ToFloat  <$> (keyword kwToFloat *> pExpr)
pAbsolute = Abs      <$> (keyword kwAbs *> pExpr)
pNot      = Not      <$> (symbol "!" *> pExpr)

addSubOperator, mulDivOperator, modPowOperator, concatOperator, booleanOperators :: Parser (Expr -> Expr -> Expr)
addSubOperator   = (Add <$ symbol "+") <|> (Sub <$ symbol "-")
mulDivOperator   = (Mul <$ symbol "*") <|> (Div <$ symbol "/")
modPowOperator   = (Mod <$ symbol "%") <|> (Pow <$ symbol "^")
concatOperator   = Concat <$ symbol "++"
booleanOperators = choice
    [ And  <$ symbol "&&"
    , Nand <$ symbol "!&"
    , Or   <$ symbol "||"
    , Nor  <$ symbol "!|"
    , Xor  <$ symbol "x|"
    , Imp  <$ symbol "->"
    , Eq   <$ symbol "=="
    ]
