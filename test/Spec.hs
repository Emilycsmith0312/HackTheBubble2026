module Main (main) where

import ClipClop.BinaryExpressions
import ClipClop.Moods
import qualified ClipClop.Parser as Lambda
import qualified ClipClop.Syntax as Lambda
import ClipClop.Value
import Control.Monad (unless)
import qualified Data.Map as Map
import System.Exit

-- | Run a block of commands the way the REPL does, without moods changing.
runAll :: String -> Env
runAll src = case parseCommands src of
  Left err   -> error (show err)
  Right cmds -> foldl step emptyEnv cmds
  where
    step env (Set t n [] e)  = case evalTop env e >>= typed t of
      Right v  -> env { envVars = Map.insert n v (envVars env) }
      Left err -> error err
    step env (Set t n ps e)  = env { envVars = Map.insert n (FunValue (Function t ps e [] False)) (envVars env) }
    step env _               = env

-- | Evaluate one expression in an environment.
ask :: Env -> String -> Either String Value
ask env src = case parseCommands src of
  Right [Print e] -> evalTop env e
  other           -> Left ("not an expression: " ++ show other)

withHerd :: (Herd -> Herd) -> Env -> Env
withHerd f env = env { envHerd = f (envHerd env) }

isLeft :: Either a b -> Bool
isLeft = either (const True) (const False)

herd :: Env
herd = runAll $ unlines
  [ "the SingularHorse a says cl1pcl0p"
  , "the SingularHorse b says cl1p"
  , "the SingularHorse double ridden by SingularHorse x says x + x"
  , "the SingularHorse add ridden by SingularHorse x and SingularHorse y says x + y"
  , "the SingularHorse useb ridden by SingularHorse x says x + b"
  , "the SingularHorse forever ridden by SingularHorse x says giddyup forever with x"
  , "the SingularHorse twice ridden by Steed f and SingularHorse x says giddyup f with giddyup f with x"
  , "the Steed adder ridden by SingularHorse n says foal SingularHorse ridden by SingularHorse m says n + m"
  , "the Steed addtwo says giddyup adder with cl1pcl0p"
  , "the SingularHorse peek ridden by SingularHorse q says x"
  , "the SingularHorse leaky ridden by SingularHorse x says giddyup peek with x"
  , "the SingularHorse sumto ridden by SingularHorse n says whinny n snort cl0p says cl0p stomp nicker says n + giddyup sumto with n - cl1p"
  ]

tests :: [(String, Bool)]
tests =
  [ ("placeholder lambda parser",
      Lambda.parseExpr "\\x. x y" == Right (Lambda.Lam "x" (Lambda.App (Lambda.Var "x") (Lambda.Var "y"))))
  -- functions
  , ("call one rider",       ask herd "giddyup double with a" == Right (IntValue 4))
  , ("call two riders",      ask herd "giddyup add with a and b" == Right (IntValue 3))
  , ("call in brackets",     ask herd "(giddyup add with a and b) + b" == Right (IntValue 4))
  , ("nested calls",         ask herd "giddyup add with (giddyup double with a) and giddyup double with b" == Right (IntValue 6))
  , ("body sees horses",     ask herd "giddyup useb with a" == Right (IntValue 3))
  , ("wrong number",         isLeft (ask herd "giddyup add with a"))
  , ("wrong rider type",     isLeft (ask herd "giddyup double with neigh"))
  -- first-class functions
  , ("function is a value",  fmap typeOf (ask herd "double") == Right (Just TFun))
  , ("pass a function",      ask herd "giddyup twice with double and a" == Right (IntValue 8))
  , ("pass a foal",          ask herd "giddyup twice with (foal SingularHorse ridden by SingularHorse n says n * n) and a" == Right (IntValue 16))
  , ("return a function",    ask herd "giddyup giddyup adder with a with b" == Right (IntValue 3))
  , ("closure keeps rider",  ask herd "giddyup addtwo with a" == Right (IntValue 4))
  , ("riders do not leak",   isLeft (ask herd "giddyup leaky with a"))
  , ("lie follows function", ask (withHerd (setMood "double" Lying) herd) "giddyup twice with double and a" == Right (IntValue 11))
  -- whinny
  , ("snort matches",        ask herd (name "b") == Right (StrValue "one"))
  , ("falls through",        ask herd (name "a") == Right (StrValue "three"))
  , ("nicker",               ask herd (name "cl1pcl0pcl0p") == Right (StrValue "lots"))
  , ("no match no nicker",   ask herd "whinny a snort b says a" == Right UnitValue)
  , ("nicker in the middle", ask herd "whinny a snort b says cl1p stomp nicker says cl0p snort cl1pcl1p says cl1pcl1p stomp" == Right (IntValue 3))
  , ("skipped arm not run",  ask herd "whinny b snort b says a stomp nicker says a / cl0p stomp" == Right (IntValue 2))
  , ("different types",      ask herd "whinny a snort \"x\" says cl1p stomp nicker says cl0p" == Right (IntValue 0))
  , ("recursion ends",       ask herd "giddyup sumto with cl1pcl0pcl0p" == Right (IntValue 10))
  , ("two nickers",          isLeft (parseCommands "whinny a nicker says a nicker says a"))
  -- lunge
  , ("lunge parses",         case parseCommands "lunge a the SingularHorse b says b + a  a + b whoa a" of
                               Right [Lunge "a" [Set _ "b" [] _, Print _], Print _] -> True
                               _ -> False)
  , ("nested lunge",         case parseCommands "lunge a lunge b a whoa whoa" of
                               Right [Lunge "a" [Lunge "b" [Print _]]] -> True
                               _ -> False)
  , ("lunge needs whoa",     isLeft (parseCommands "lunge a a + b"))
  , ("readStory parses",     case parseCommands "readStory \"a.dotclipclop\" a" of
                               Right [ReadStory (Val (StrValue "a.dotclipclop")), Print _] -> True
                               _ -> False)
  -- comments
  , ("comment at the end",   ask herd "a + b pssssst not this" == Right (IntValue 3))
  , ("comment in a string",  ask herd "\"pssssst\"" == Right (StrValue "pssssst"))
  , ("comment-only line",    fmap length (parseCommands "a pssssst one\npssssst two\nb") == Right 2)
  , ("riding a variable",    isLeft (ask herd "giddyup a with b"))
  , ("endless recursion",    isLeft (ask herd "giddyup forever with a"))
  -- moods
  , ("sulking refuses",      isLeft (ask (withHerd (setMood "a" Sulking) herd) "a + b"))
  , ("ran away refuses",     isLeft (ask (withHerd (setMood "a" RanAway) herd) "a + b"))
  , ("lying lies",           ask (withHerd (setMood "a" Lying) herd) "a" == Right (IntValue 3))
  , ("lying function lies",  ask (withHerd (setMood "double" Lying) herd) "giddyup double with a" == Right (IntValue 5))
  , ("sulking function",     isLeft (ask (withHerd (setMood "double" Sulking) herd) "giddyup double with a"))
  , ("rider is not the horse", ask (withHerd (setMood "x" Sulking) herd) "giddyup double with a" == Right (IntValue 4))
  , ("feud blocks",          isLeft (ask (withHerd (startFeud "b" "a") herd) "a + b"))
  , ("feud through a body",  isLeft (ask (withHerd (startFeud "a" "b") herd) "giddyup useb with a"))
  , ("feud alone is fine",   ask (withHerd (startFeud "a" "b") herd) "a + a" == Right (IntValue 4))
  , ("made up",              ask (withHerd (endFeud "a" "b" . startFeud "a" "b") herd) "a + b" == Right (IntValue 3))
  , ("same seed same moods", fst (stirUp names emptyHerd (mkGen 7)) == fst (stirUp names emptyHerd (mkGen 7)))
  , ("moods do happen",      any (\s -> fst (stirUp names emptyHerd (mkGen s)) /= emptyHerd) [1 .. 50])
  , ("lies keep their type", and [ typeOf (lieAbout v) == typeOf v
                                 | v <- [IntValue 1, FloatValue 1, BoolValue True, CharValue 'a', StrValue "aba", StrValue "ab", UnitValue] ])
  , ("lies are lies",        and [ lieAbout v /= v | v <- [IntValue 1, FloatValue 1, BoolValue True, CharValue 'a', StrValue "aba", StrValue "ab", StrValue ""] ])
  ]
  where
    names = ["a", "b", "double"]
    name x = "whinny " ++ x ++ " snort cl1p says \"one\" stomp snort cl1pcl0p says \"two\""
          ++ " snort cl1pcl1p says \"three\" stomp nicker says \"lots\" stomp"

main :: IO ()
main = do
  let failed = [name | (name, ok) <- tests, not ok]
  mapM_ (putStrLn . ("FAILED: " ++)) failed
  unless (null failed) exitFailure
  putStrLn ("ok (" ++ show (length tests) ++ " tests)")
