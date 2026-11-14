module ClipClop.BinaryExpressions where

import ClipClop.Moods
import ClipClop.Value
import Control.Monad (foldM, when, zipWithM)
import Data.List (isPrefixOf, nub)
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Char (isAlphaNum, toUpper, toLower)
import qualified Data.Map as Map
import Text.Parsec hiding (Empty)
import Text.Parsec.String (Parser)

-- Command data type
data Command = Set Type Name [(Type, Name)] Expr
               -- ^ assign an expression to a name: the {type} {name} says {expr}
               -- With parameters it is short for assigning a function (see 'Lam'):
               --   the {type} {name} ridden by {type} {param} and {type} {param} says {expr}
             | Print Expr         -- evaluate an expression and print the result
             | Treat Name         -- treat {name}: a sulking horse stops ignoring you
             | Interrogate Name   -- interrogate {name}: a lying horse tells the truth again
             | Honey Name         -- honey {name}: coax back a horse that ran away
             | MakeUp Name Name   -- makeup {name} and {name}: end a feud between two horses
             | ReadStory Expr     -- readStory {expr}: run every line of a .dotclipclop file
             | Lunge Name [Command]
               -- ^ loop: lunge {name} {command} {command} ... whoa
               -- Runs the commands once for every step it takes to walk the
               -- horse's number to zero (see 'lunge' in the REPL for the catches).
  deriving Show

-- | Everything an expression is evaluated against.
data Env = Env
  { envVars   :: Map Name Value -- the horses (functions are horses too)
  , envHerd   :: Herd           -- moods and feuds (see ClipClop.Moods)
  , envLocals :: Map Name Value -- the riders, while inside a function body
  , envDepth  :: Int            -- how many function calls deep we are
  }

emptyEnv :: Env
emptyEnv = Env Map.empty emptyHerd Map.empty 0

-- | How deep function calls may nest before the horse gives up.
maxDepth :: Int
maxDepth = 200

-- | Is there a horse with this name?
isHorse :: Name -> Env -> Bool
isHorse n env = Map.member n (envVars env)

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
typeOf (FunValue _)   = Just TFun
typeOf (ListValue _)  = Just THerd
typeOf (ErrValue _)   = Nothing

-- Eval is used to evaluate the expression given - recursively calls itself to evaluate the expression
eval :: Env -> -- Variables, functions and moods
        Expr -> -- Expression to evaluate
        Either String Value -- Returns either an error (such as missing variables) or the result
eval vars (Val x) = Right x -- for values, just give the value directly 
eval vars (Var x) = case Map.lookup x (envLocals vars) of -- variables
                        Just v  -> Right v -- a rider: not a horse, so it has no moods
                        Nothing -> do
                          xVal <- valueOf x (envVars vars) -- find in tree
                          inTheMood x vars
                          return (asToldBy x vars xVal)
eval vars (Lam t ps body) = -- anonymous function: remembers the riders around it
                        Right (FunValue (Function t ps body (Map.toList (envLocals vars)) False))
eval vars (Call f args) = do -- function application
                        callee <- eval vars f
                        argVals <- mapM (eval vars) args
                        ride vars (nameOf f) callee argVals
-- HERDS (lists) --
eval vars (HerdOf xs) = ListValue <$> mapM (\x -> eval vars x >>= noErr) xs
eval vars (Leader h) = do
                        xs <- herdOf "leader" vars h
                        case xs of
                          x : _ -> Right x
                          []    -> Left "the paddock is empty: nobody is the leader"
eval vars (Stragglers h) = do
                        xs <- herdOf "stragglers" vars h
                        case xs of
                          _ : rest -> Right (ListValue rest)
                          []       -> Left "the paddock is empty: there are no stragglers"
eval vars (Headcount h) = IntValue . length <$> herdOf "headcount" vars h
eval vars (Cut n h) = do -- the first member is number cl1p
                        place <- eval vars n
                        xs <- herdOf "cut" vars h
                        case place of
                          IntValue k | k >= 1 && k <= length xs -> Right (xs !! (k - 1))
                                     | otherwise -> Left ("there is no horse number " ++ show place ++ " in a herd of " ++ show (IntValue (length xs)))
                          ErrValue err -> Left err
                          _ -> Left ("horses are counted with a " ++ typeName TInt)
eval vars (Stampede f h) = do -- map
                        fn <- eval vars f
                        xs <- herdOf "stampede" vars h
                        ListValue <$> mapM (\x -> ride vars (nameOf f) fn [x]) xs
eval vars (Cull f h) = do -- filter, keeping the members the function says neigh to
                        fn <- eval vars f
                        xs <- herdOf "cull" vars h
                        kept <- mapM (\x -> ride vars (nameOf f) fn [x] >>= stays x) xs
                        return (ListValue (concat kept))
  where stays x (BoolValue gone) = Right [x | not gone]
        stays _ _                = Left ("to cull, " ++ nameOf f ++ " must say an " ++ typeName TBool)
eval vars (Roundup f z h) = do -- fold from the left: f (f (f start a) b) c
                        fn <- eval vars f
                        start <- eval vars z >>= noErr
                        xs <- herdOf "roundup" vars h
                        foldM (\acc x -> ride vars (nameOf f) fn [acc, x]) start xs
eval vars (Whinny x arms) = do -- case expression
                        (_, bodies) <- whinnyPath vars x arms
                        results <- mapM (\b -> eval vars b >>= noErr) bodies
                        return (if null results then UnitValue else last results)
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
eval vars (Electrocute x) = do 
-- flip the bits of an integer/double
                        xNum <- eval vars x
                        Right (electrocuteValues xNum) -- call from Value.hs
eval vars (Shout x) = case eval vars x of
                        Right (StrValue s) -> Right(StrValue(map toUpper s))
                        Right (FloatValue f) -> Right(StrValue(map toUpper (show $ FloatValue f)))
                        Right (IntValue i) -> Right(StrValue(map toUpper (show $ IntValue i)))
                        _ -> Left "parse error"
eval vars (Whisper x) = case eval vars x of
                        Right (StrValue s) -> Right(StrValue(map toLower s))
                        Right (FloatValue f) -> Right(StrValue(show f))
                        Right (IntValue i) -> Right(StrValue(show i))
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

-- ELECTROCUTION SIDE EFFECTS --
-- 'eval' is pure, so electrocuting a string or character only gives its value
-- there; the consequences below are worked out separately and carried out by the REPL.

data Effect = KickedInHead (Maybe Name) -- a string horse was offended; you are concussed
            | HorseDied (Maybe Name)    -- a character horse died
  deriving (Show, Eq)

-- | How many lines a kick to the head leaves you concussed for.
concussionLines :: Int
concussionLines = 5

-- | Every electrocution side effect in an expression, in left-to-right order.
-- Effects inside the body of a function happen when it is successfully called.
electrocuteEffects :: Env -> Expr -> [Effect]
electrocuteEffects vars e = concatMap (electrocuteEffects vars) (evaluated vars e) ++ here e
  where
    here (Electrocute x) = case eval vars x of
      Right (StrValue _)  -> [KickedInHead (horseName x)]
      Right (CharValue _) -> [HorseDied (horseName x)]
      _                   -> []
    here c@(Call f args)
      | Right _ <- eval vars c
      , Right (FunValue fn) <- eval vars f
      , Right argVals <- mapM (eval vars) args
      = electrocuteEffects (mount fn (zip (map snd (fnParams fn)) argVals) vars) (fnBody fn)
    here _ = []
    horseName (Var n) | Map.notMember n (envLocals vars) = Just n -- riders are not horses
    horseName _       = Nothing

-- | The message shown for an effect.
effectMessage :: Effect -> String
effectMessage (KickedInHead n) = "You were kicked in the head because " ++ maybe "a nameless horse" id n
                              ++ " was offended! You are concussed for the next five lines."
effectMessage (HorseDied n)    = maybe "A nameless horse" id n ++ " has died."

-- | The sub-expressions that are actually evaluated when an expression is.
-- Only a whinny differs from 'children': the arms it skips are never evaluated.
evaluated :: Env -> Expr -> [Expr]
evaluated env (Whinny x arms) = x : either (const []) (uncurry (++)) (whinnyPath env x arms)
evaluated _   e               = children e

-- | The direct sub-expressions of an expression. The body of a foal is not
-- one of them: it is not evaluated until the foal is ridden.
children :: Expr -> [Expr]
children e = case e of
  Add a b -> [a, b];    Sub a b -> [a, b];  Mul a b -> [a, b];  Div a b -> [a, b]
  Mod a b -> [a, b];    Pow a b -> [a, b];  Concat a b -> [a, b]
  And a b -> [a, b];    Nand a b -> [a, b]; Or a b -> [a, b];   Xor a b -> [a, b]
  Nor a b -> [a, b];    Imp a b -> [a, b];  Eq a b -> [a, b]
  Abs a -> [a];         Not a -> [a];       ToString a -> [a];  ToInt a -> [a]
  ToFloat a -> [a];     ToBool a -> [a];    Electrocute a -> [a]
  Shout a -> [a];       Whisper a -> [a]
  Val _ -> [];          Var _ -> []
  Call f args -> f : args
  Lam _ _ _ -> []
  HerdOf xs -> xs
  Leader a -> [a];      Stragglers a -> [a]; Headcount a -> [a]
  Cut a b -> [a, b];    Stampede a b -> [a, b]; Cull a b -> [a, b]
  Roundup a b c -> [a, b, c]
  Whinny x arms -> x : concat [ maybe [] (: []) p ++ [b] | Arm p b _ <- arms ]

-- | Look a variable up in the environment.
valueOf :: Name -> Map Name Value -> Either String Value
valueOf x vars = maybe (Left ("variable not defined: " ++ x)) Right (Map.lookup x vars)

-- | What to call a function in an error message.
nameOf :: Expr -> String
nameOf (Var x) = x
nameOf _       = "a foal"

-- | Ride a function value with some riders. Used by giddyup and by the herd
-- operations that take a function.
ride :: Env -> String -> Value -> [Value] -> Either String Value
ride vars who callee argVals = do
    fn <- case callee of
            FunValue fn  -> Right fn
            ErrValue err -> Left err
            _            -> Left (who ++ " cannot be ridden")
    when (length argVals /= length (fnParams fn)) $
      Left (who ++ " is ridden by " ++ show (length (fnParams fn)) ++ " but was given " ++ show (length argVals))
    when (envDepth vars >= maxDepth) $ Left (who ++ " collapsed from exhaustion")
    riders <- zipWithM (\(t, p) v -> (,) p <$> typed t v) (fnParams fn) argVals
    result <- eval (mount fn riders vars) (fnBody fn) >>= typed (fnReturn fn)
    return (if fnLies fn then lieAbout result else result)

-- | Evaluate an expression that must be a herd, for the named operation.
herdOf :: String -> Env -> Expr -> Either String [Value]
herdOf what vars h = do
    v <- eval vars h
    case v of
      ListValue xs -> Right xs
      ErrValue err -> Left err
      _            -> Left (what ++ " needs a " ++ typeName THerd)

-- | Like 'checkType', but an error value is reported as the error it holds.
typed :: Type -> Value -> Either String Value
typed _ (ErrValue err) = Left err
typed t v              = checkType t v

-- | An error value is reported as the error it holds.
noErr :: Value -> Either String Value
noErr (ErrValue err) = Left err
noErr v              = Right v

-- | The environment inside a function body: the horses, plus the riders the
-- function captured when it was made, plus the riders it was just given.
mount :: Function -> [(Name, Value)] -> Env -> Env
mount fn riders env = env
  { envLocals = Map.fromList (fnCaptured fn ++ riders) -- later entries win
  , envDepth  = envDepth env + 1
  }

-- | Work out what a whinny evaluates: the snort values it tests, in order,
-- and the bodies it then runs.
--
-- Like a C switch: the first snort equal to the whinnied value is chosen, or
-- else the nicker; its body runs, and so does every body after it until one
-- ends in stomp. The value of the whinny is the value of the last body run
-- (unit if none ran). Values of different types are simply not equal.
whinnyPath :: Env -> Expr -> [Arm] -> Either String ([Expr], [Expr])
whinnyPath env x arms = do
    v <- eval env x >>= noErr
    (tested, found) <- seek v [] arms
    return (tested, run (maybe (dropWhile isSnort arms) id found))
  where
    seek _ acc [] = Right (reverse acc, Nothing)
    seek v acc (Arm Nothing _ _ : rest) = seek v acc rest
    seek v acc (arm@(Arm (Just p) _ _) : rest) = do
      pv <- eval env p >>= noErr
      if pv == v then Right (reverse (p : acc), Just (arm : rest)) else seek v (p : acc) rest
    isSnort (Arm p _ _) = p /= Nothing
    run [] = []
    run (Arm _ b stomp : rest) = b : if stomp then [] else run rest

-- HORSE MOODS (see ClipClop.Moods) --

-- | A sulking or missing horse refuses to be evaluated. (Only ever asked about
-- horses: riders are looked up first and never get here.)
inTheMood :: Name -> Env -> Either String ()
inTheMood x env = case moodOf x (envHerd env) of
  Just Sulking -> Left (x ++ " is ignoring you")
  Just RanAway -> Left (x ++ " has run away")
  _            -> Right ()

-- | A lying horse changes the value it gives.
asToldBy :: Name -> Env -> Value -> Value
asToldBy x env v
  | moodOf x (envHerd env) == Just Lying = lieAbout v
  | otherwise                            = v

-- | Every horse an expression might use, including those used inside the
-- bodies of the function horses it mentions.
horsesIn :: Env -> Expr -> Set Name
horsesIn env = go Set.empty []
  where
    go seen riders e = case e of
      Var x | x `elem` riders -> Set.empty
            | otherwise       -> Set.insert x $ case Map.lookup x (envVars env) of
                Just (FunValue fn) | x `Set.notMember` seen
                  -> go (Set.insert x seen) (map snd (fnParams fn) ++ map fst (fnCaptured fn)) (fnBody fn)
                _ -> Set.empty
      Lam _ ps body -> go seen (map snd ps ++ riders) body
      _             -> Set.unions (map (go seen riders) (children e))

-- | Two feuding horses refuse to take part in the same evaluation.
checkFeuds :: Env -> Expr -> Either String ()
checkFeuds env e = case [ (a, b) | (a, b) <- Set.toList (herdFeuds (envHerd env))
                                 , a `Set.member` used, b `Set.member` used ] of
  (a, b) : _ -> Left (a ++ " and " ++ b ++ " are not speaking to each other")
  []         -> Right ()
  where used = horsesIn env e

-- | Evaluate a whole expression as a command would: feuds first, then 'eval'.
evalTop :: Env -> Expr -> Either String Value
evalTop env e = checkFeuds env e >> eval env e

-- COMMENTS --

kwComment :: String
kwComment = "pssssst" -- the rest of the line is a comment (exactly five s's)

-- | Blank out every comment, leaving line breaks alone so that parse errors
-- still give the right line and column. A pssssst inside a string or
-- character literal, or on the end of a longer word, is not a comment.
stripComments :: String -> String
stripComments = go False
  where
    go _ [] = []
    go _ ('"' : cs) = let (str, rest) = break (== '"') cs
                      in '"' : str ++ take 1 rest ++ go False (drop 1 rest)
    go _ ('\'' : c : '\'' : cs) = '\'' : c : '\'' : go False cs
    go inWord s@(c : cs)
      | not inWord, kwComment `isPrefixOf` s = let (comment, rest) = break (== '\n') s
                                               in map (const ' ') comment ++ go False rest
      | otherwise = c : go (isAlphaNum c) cs

-- PARSING (Parsec) --

-- | Parse a complete line of input as a command.
parseCommand :: String -> Either ParseError Command
parseCommand = parse (pCommand <* eof) "<repl>" . stripComments

-- | Parse a block of one or more commands, e.g. several lines joined with '\'.
-- Each command ends where the next one begins; an expression may span lines.
parseCommands :: String -> Either ParseError [Command]
parseCommands = parse (many1 pCommand <* eof) "<repl>" . stripComments

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

kwTrue, kwFalse, kwPrint, kwAbs, kwToString, kwToFloat, kwToInt, kwToBool, kwShout, kwWhisper :: String
kwTrue     = "neighneigh"     -- true
kwFalse    = "neigh"          -- false
kwPrint    = "wellsaysomething" -- print
kwShout    = "speakup"    -- to upper
kwWhisper  = "quietdown"  -- to lower
kwAbs      = "geld"           -- abs
kwToString = "toSuffolkPunch" -- toString
kwToFloat  = "maretosis"      -- toFloat
kwToInt    = "amalgamare"     -- toInt
kwToBool   = "addSpots"       -- toBoolean
kwElectrocute :: String
kwElectrocute = "electrocute" -- flip the bits of a number
kwThe, kwSays :: String
kwThe      = "the"            -- start of a variable declaration
kwSays     = "says"           -- between the variable name and its value
kwRidden, kwBy, kwAnd, kwCall, kwWith :: String
kwRidden   = "ridden"         -- "ridden by" starts a function's parameters
kwBy       = "by"
kwAnd      = "and"            -- separates parameters, arguments and feuding horses
kwCall     = "giddyup"        -- apply a function
kwWith     = "with"           -- between a function's name and its arguments
kwFoal, kwWhinny, kwSnort, kwNicker, kwStomp :: String
kwFoal     = "foal"           -- an anonymous function
kwWhinny   = "whinny"         -- switch
kwSnort    = "snort"          -- case
kwNicker   = "nicker"         -- default
kwStomp    = "stomp"          -- break
kwHerd, kwOf, kwPaddock, kwLeader, kwStragglers, kwHeadcount, kwCut, kwFrom :: String
kwHerd       = "herd"         -- "herd of" starts a list
kwOf         = "of"
kwPaddock    = "paddock"      -- the empty list
kwLeader     = "leader"       -- head
kwStragglers = "stragglers"   -- tail
kwHeadcount  = "headcount"    -- length
kwCut        = "cut"          -- index: cut {number} from {herd}
kwFrom       = "from"
kwStampede, kwThrough, kwCull, kwRoundup :: String
kwStampede   = "stampede"     -- map: stampede {function} through {herd}
kwThrough    = "through"
kwCull       = "cull"         -- filter out: cull {function} from {herd}
kwRoundup    = "roundup"      -- fold: roundup {function} from {start} through {herd}
kwReadStory, storyExtension :: String
kwReadStory    = "readStory"    -- run the lines of a story file
storyExtension = ".dotclipclop" -- what a story file's name must end in
kwLunge, kwWhoa :: String
kwLunge    = "lunge"          -- start of a loop
kwWhoa     = "whoa"           -- end of a loop
kwTreat, kwInterrogate, kwHoney, kwMakeUp :: String
kwTreat       = "treat"       -- give a sulking horse a treat
kwInterrogate = "interrogate" -- make a lying horse confess
kwHoney       = "honey"       -- coax back a horse that ran away
kwMakeUp      = "makeup"      -- force two feuding horses to make up

-- | Words that cannot be used as variable names.
reserved :: [String]
reserved = [kwTrue, kwFalse, kwPrint, kwAbs, kwToString, kwToFloat, kwToInt, kwToBool, kwThe, kwSays, kwElectrocute, kwShout, kwWhisper]
        ++ [kwRidden, kwBy, kwAnd, kwCall, kwWith, kwTreat, kwInterrogate, kwHoney, kwMakeUp]
        ++ [kwFoal, kwWhinny, kwSnort, kwNicker, kwStomp, kwLunge, kwWhoa, kwReadStory]
        ++ [kwHerd, kwOf, kwPaddock, kwLeader, kwStragglers, kwHeadcount, kwCut, kwFrom]
        ++ [kwStampede, kwThrough, kwCull, kwRoundup]
        ++ ["duplicateHorse"] -- reserved for future use

identifier :: Parser Name
identifier = try $ do
    name <- lexeme ((:) <$> lower <*> many alphaNum)
    if name `elem` reserved then unexpected ("keyword " ++ show name) else return name

-- Parse the command
pCommand :: Parser Command
pCommand = spaces *> (pSet <|> pHorseCare <|> pLunge <|> pReadStory <|> pPrint)

-- run a story file
-- readStory {expression}      (the expression must be a SuffolkPunch: the file's name)
pReadStory :: Parser Command
pReadStory = ReadStory <$> (keyword kwReadStory *> pExpr)

-- loop
-- lunge {name} {command} {command} ... whoa
pLunge :: Parser Command
pLunge = Lunge <$> (keyword kwLunge *> identifier) <*> (many pCommand <* keyword kwWhoa)

-- looking after the horses' moods
-- treat {name} | interrogate {name} | honey {name} | makeup {name} and {name}
pHorseCare :: Parser Command
pHorseCare = choice
    [ Treat       <$> (keyword kwTreat *> identifier)
    , Interrogate <$> (keyword kwInterrogate *> identifier)
    , Honey       <$> (keyword kwHoney *> identifier)
    , MakeUp      <$> (keyword kwMakeUp *> identifier) <*> (keyword kwAnd *> identifier)
    ]

-- define variable or function
-- the {type} {var name} says {expression}
-- the {type} {function name} ridden by {type} {param} and {type} {param} says {expression}
pSet :: Parser Command
pSet = do
    keyword kwThe
    t <- pType
    name <- identifier
    params <- option [] pRiders
    keyword kwSays
    Set t name params <$> pExpr

-- the riders (parameters) of a function
-- ridden by {type} {param} and {type} {param}
pRiders :: Parser [(Type, Name)]
pRiders = do
    keyword kwRidden
    keyword kwBy
    params <- sepBy1 ((,) <$> pType <*> identifier) (keyword kwAnd)
    when (length (nub (map snd params)) /= length params) $
      unexpected "two riders with the same name"
    return params

-- an anonymous function
-- foal {return type} ridden by {type} {param} and {type} {param} says {expression}
-- The body runs as far as it can, so wrap the foal in brackets to end it early.
pLam :: Parser Expr
pLam = Lam <$> (keyword kwFoal *> pType) <*> pRiders <*> (keyword kwSays *> pExpr)

-- apply a function
-- giddyup {function} with {expression} and {expression}
-- The function is a name, a bracketed expression, or another giddyup.
-- Each argument runs as far as it can, so wrap the call in brackets to use its
-- result in a bigger expression: (giddyup double with x) + cl1p
pCall :: Parser Expr
pCall = Call <$> (keyword kwCall *> pFactor) <*> (keyword kwWith *> sepBy1 pExpr (keyword kwAnd))

-- case expression
-- whinny {expression}
--   snort {expression} says {expression} stomp
--   snort {expression} says {expression}          (no stomp: falls through)
--   nicker says {expression} stomp
pWhinny :: Parser Expr
pWhinny = do
    keyword kwWhinny
    x <- pExpr
    arms <- many1 pArm
    when (length [() | Arm Nothing _ _ <- arms] > 1) $ unexpected "a second nicker"
    return (Whinny x arms)
  where
    pArm = Arm <$> (snort <|> nicker) <*> (keyword kwSays *> pExpr) <*> option False (True <$ keyword kwStomp)
    snort  = Just <$> (keyword kwSnort *> pExpr)
    nicker = Nothing <$ keyword kwNicker

pType :: Parser Type
pType = choice [t <$ keyword n | (t, n) <- typeNames] <?> "a type"

-- print 
pPrint :: Parser Command
pPrint = optional (keyword kwPrint) *> (Print <$> pExpr)

pExpr :: Parser Expr
pExpr = pAbsolute <|> pNot <|> pToString <|> pToBool <|> pToNum <|> pElectrocute <|> pHerdPrefix <|> pTerm <|> pShout <|> pWhisper

-- HERDS (lists) --

-- leader / stragglers / headcount: like geld, each applies to everything after it
pHerdPrefix :: Parser Expr
pHerdPrefix = choice
    [ Leader     <$> (keyword kwLeader *> pExpr)
    , Stragglers <$> (keyword kwStragglers *> pExpr)
    , Headcount  <$> (keyword kwHeadcount *> pExpr)
    ]

-- The herd forms with more than one part. Like giddyup, they can be used as an
-- operand, and each part runs as far as it can (up to the next keyword).
-- herd of {expression} and {expression}
-- paddock
-- cut {number} from {herd}
-- stampede {function} through {herd}
-- cull {function} from {herd}
-- roundup {function} from {start} through {herd}
pHerd :: Parser Expr
pHerd = choice
    [ HerdOf   <$> (keyword kwHerd *> keyword kwOf *> sepBy1 pExpr (keyword kwAnd))
    , Val (ListValue []) <$ keyword kwPaddock
    , Cut      <$> (keyword kwCut *> pExpr) <*> (keyword kwFrom *> pExpr)
    , Stampede <$> (keyword kwStampede *> pExpr) <*> (keyword kwThrough *> pExpr)
    , Cull     <$> (keyword kwCull *> pExpr) <*> (keyword kwFrom *> pExpr)
    , Roundup  <$> (keyword kwRoundup *> pExpr) <*> (keyword kwFrom *> pExpr) <*> (keyword kwThrough *> pExpr)
    ]

pTerm :: Parser Expr
pTerm = chainl1 pFactor (booleanOperators <|> concatOperator <|> modPowOperator <|> mulDivOperator <|> addSubOperator)

pFactor :: Parser Expr
pFactor = pValue <|> pCall <|> pLam <|> pWhinny <|> pHerd <|> pVariable <|> pParenExpr

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

pToString, pToBool, pToNum, pToInt, pToFloat, pAbsolute, pNot, pElectrocute, pShout, pWhisper :: Parser Expr
pElectrocute = Electrocute <$> (keyword kwElectrocute *> pExpr)
pShout = Shout <$> (keyword kwShout *> pExpr)
pWhisper = Whisper <$> (keyword kwWhisper *> pExpr)
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
