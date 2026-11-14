-- | The interactive read-eval-print loop.
--
-- Line editing (arrow keys, history, Ctrl-R, emacs keys, tab completion of
-- commands) comes from haskeline. Enter runs the line. Pasting several lines
-- runs them one after another. Ctrl-C cancels the line, Ctrl-D quits.
module ClipClop.REPL (repl, runStories) where

import ClipClop.BinaryExpressions
import ClipClop.Moods
import ClipClop.Sound (Sounds, exprCues, finishSounds, initSounds, playCues, textCues)
import ClipClop.Value
import Control.Exception (IOException, try)
import Control.Monad (foldM)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isSpace)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (dropWhileEnd, intercalate, isPrefixOf, isSuffixOf)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Console.Haskeline
import System.Directory (getHomeDirectory)
import System.Environment (lookupEnv)
import System.IO
import Text.Read (readMaybe)

-- | Everything the REPL remembers between lines.
data ReplState = ReplState
  { env       :: Env    -- the horses: variables, functions and their moods
  , concussed :: Int    -- lines left during which output prints backwards
  , gen       :: Gen    -- random numbers for the horses' moods
  , calm      :: Bool   -- True if the horses never change mood (CLIPCLOP_CALM is set)
  , sounds    :: Sounds -- sound effects
  , telling   :: [FilePath] -- the stories being read right now, innermost first
  }

repl :: IO ()
repl = do
  hSetBuffering stdout NoBuffering
  home <- getHomeDirectory
  let settings :: Settings IO
      settings = (defaultSettings :: Settings IO)
        { complete    = completer
        , historyFile = Just (home ++ "/.clipclop_history")
        }
  st <- newState
  runInputT settings . withInterrupt $ do
    outputStrLn "\nClip Clop REPL. Type :help for commands, :shoo or :q to quit.\n"
    loop st

-- | A fresh state: no horses, moods seeded from the clock.
newState :: IO ReplState
newState = do
  seed <- round . (* 1000000) <$> getPOSIXTime
  isCalm <- (/= Nothing) <$> lookupEnv "CLIPCLOP_CALM"
  snd' <- initSounds
  return (ReplState emptyEnv 0 (mkGen seed) isCalm snd' [])

-- | Run story files without the REPL, one after another with the same horses
-- (this is what @bubble2026 FILE...@ does), then wait for the sounds to finish.
runStories :: [FilePath] -> IO ()
runStories files = do
  hSetBuffering stdout NoBuffering
  st <- newState
  st' <- foldM readStory st files
  finishSounds (sounds st')

------------------------------------------------------------------------------
-- The loop and the : commands

commandNames :: [String]
commandNames = [":help", ":vars", ":load", ":reset", ":seed", ":quit", ":q"]

-- | Tab completes the : commands.
completer :: CompletionFunc IO
completer = completeWord Nothing " \t" $ \w ->
  return [simpleCompletion c | ":" `isPrefixOf` w, c <- commandNames, w `isPrefixOf` c]

-- | Read a line, run it, and loop with the updated state.
loop :: ReplState -> InputT IO ()
loop st = do
  r <- handleInterrupt (return Nothing) (Just <$> getInputLine "clipclop> ")
  case r of
    Nothing         -> outputStrLn "^C" >> loop st   -- Ctrl-C
    Just Nothing    -> outputStrLn ""                -- Ctrl-D
    Just (Just src)
      | continues src -> continuation st [dropBackslash src]
      | otherwise     -> dispatch st src

-- | Keep reading "...>" lines while each one ends in a backslash, then run them
-- as one block (which may hold several statements, or one statement split over
-- lines). Ctrl-C abandons the block; Ctrl-D abandons it and quits.
continuation :: ReplState -> [String] -> InputT IO ()
continuation st acc = do
  r <- handleInterrupt (return Nothing) (Just <$> getInputLine "     ...> ")
  case r of
    Nothing      -> outputStrLn "^C" >> loop st
    Just Nothing -> outputStrLn "\nUnfinished definition discarded."
    Just (Just l)
      | continues l -> continuation st (acc ++ [dropBackslash l])
      | otherwise   -> liftIO (run st (joinBlock (acc ++ [l]))) >>= loop

-- | Does this line end in a continuation backslash (ignoring trailing spaces)?
continues :: String -> Bool
continues l = case dropWhileEnd isSpace (stripComments l) of
  "" -> False
  t  -> last t == '\\'

-- | Remove the continuation backslash from the end of a line.
dropBackslash :: String -> String
dropBackslash = init . dropWhileEnd isSpace . stripComments

-- | Group each line ending in a backslash with the line after it into one block.
joinContinuations :: [String] -> [String]
joinContinuations = go []
  where
    go acc (l : ls)
      | continues l = go (acc ++ [dropBackslash l]) ls
      | otherwise   = joinBlock (acc ++ [l]) : go [] ls
    go [] []  = []
    go acc [] = [joinBlock acc] -- file ended mid-block: run what there is

-- | Lines stay separate inside a block, so parse errors give the right line number.
joinBlock :: [String] -> String
joinBlock = intercalate "\n"

-- | A line starting with ':' is a REPL command. Anything else is run as code,
-- one line at a time (so pasted text containing several lines still works).
dispatch :: ReplState -> String -> InputT IO ()
dispatch st src = case words src of
  []                              -> loop st
  [":shoo"]                       -> shoo st
  (c : args) | ":" `isPrefixOf` c -> command st c args
  _                               -> liftIO (runLines st src) >>= loop

command :: ReplState -> String -> [String] -> InputT IO ()
command st c args = case (c, args) of
  (":q", _)      -> bye
  (":quit", _)   -> bye
  (":help", _)   -> outputStr helpText >> loop st
  (":vars", _)   -> outputStr (showVars st) >> loop st
  (":reset", _)  -> outputStrLn "All horses released." >> loop st { env = emptyEnv, concussed = 0 }
  (":seed", [n]) | Just k <- readMaybe n -> loop st { gen = mkGen k }
  (":seed", _)   -> outputStrLn "Usage: :seed NUMBER" >> loop st
  (":load", [f]) -> loadFile st f >>= loop
  (":load", _)   -> outputStrLn "Usage: :load FILE" >> loop st
  _              -> outputStrLn ("Unknown command " ++ c ++ ". Try :help") >> loop st
  where bye = liftIO (say st "Bye.")

-- | Say bye to every horse that is still defined, then quit.
shoo :: ReplState -> InputT IO ()
shoo st = liftIO (mapM_ (\name -> say st ("Bye " ++ name)) (horseNames (env st)))

-- | Every horse.
horseNames :: Env -> [Name]
horseNames e = Map.keys (envVars e)

-- | Run every non-blank line of the text as its own statement
-- (lines ending in a backslash continue onto the next line).
runLines :: ReplState -> String -> IO ReplState
runLines st src = foldM run st (filter (not . all isSpace) (joinContinuations (lines src)))

-- | Run every non-blank line of a file.
loadFile :: ReplState -> FilePath -> InputT IO ReplState
loadFile st f = do
  r <- liftIO (try (readFile f) :: IO (Either IOException String))
  case r of
    Left _    -> outputStrLn ("Could not read " ++ f) >> return st
    Right src -> liftIO (runLines st src)

-- | Assumes 'Name' is a String alias; unwrap it here if it is a newtype.
showVars :: ReplState -> String
showVars st
  | null (horseNames e) = "  (no horses yet)\n"
  | otherwise = unlines [ "  " ++ n ++ " = " ++ show (asToldBy n e v) -- liars lie here too
                        | (n, v) <- Map.toList (envVars e) ]
  where e = env st

helpText :: String
helpText = unlines
  [ "Commands"
  , "  :help        show this help"
  , "  :vars        list the horses (variables)"
  , "  :load FILE   run each line of FILE"
  , "  :reset       release all the horses"
  , "  :seed N      restart the horses' moods from seed N (same seed, same moods)"
  , "  :q           quit"
  , "  :shoo        say bye to every horse, then quit"
  , "Horses"
  , "  the TYPE NAME says EXPR                          a horse that says a value"
  , "  the TYPE NAME ridden by TYPE A and TYPE B says EXPR   a horse that must be ridden"
  , "  giddyup NAME with EXPR and EXPR                  ride it"
  , "  foal TYPE ridden by TYPE A says EXPR             a horse too young to have a name"
  , "  whinny EXPR snort EXPR says EXPR stomp nicker says EXPR stomp"
  , "  lunge NAME COMMAND COMMAND ... whoa              go round in circles until NAME says cl0p"
  , "  herd of EXPR and EXPR, paddock, leader, stragglers, headcount, cut N from HERD"
  , "  stampede F through HERD, cull F from HERD, roundup F from START through HERD"
  , "  readStory \"FILE.dotclipclop\"                    run each line of a story file"
  , "  pssssst the rest of the line is a secret"
  , "Horse care"
  , "  treat NAME, interrogate NAME, honey NAME, makeup NAME and NAME"
  , "Keys"
  , "  up/down history, Ctrl-R search, Tab completes commands,"
  , "  Ctrl-C cancels the line you are typing, Ctrl-D quits."
  , "  Pasting several lines runs them one after another."
  , "  End a line with \\ to keep going on the next line: one statement"
  , "  split over several lines, or several statements run together."
  ]

------------------------------------------------------------------------------
-- Unchanged from before

-- | Print output, backwards if concussed.
say :: ReplState -> String -> IO ()
say st s
  | concussed st > 0 = putStrLn (unlines' (map reverse (lines s)))
  | otherwise        = putStrLn s
  where unlines' = foldr1 (\a b -> a ++ "\n" ++ b) . orBlank
        orBlank [] = [""]
        orBlank ls = ls

-- | Run a line, or a block of lines joined with '\': parse every command in it,
-- then run them in order. Nothing runs if any part of the block fails to parse.
run :: ReplState -> String -> IO ReplState
run st block
 | all isSpace (stripComments block) = return st -- nothing but comments
 | otherwise = case parseCommands block of
  Left err   -> do
    say st ("Parse error: " ++ show err)
    return st { concussed = max 0 (concussed st - 1) }
  Right cmds -> foldM runCommand st cmds

-- | Run one command. Each command counts as one line of concussion.
runCommand :: ReplState -> Command -> IO ReplState
runCommand st cmd = case cmd of
  Set t name ps@(_ : _) body -> do -- a function: nothing is evaluated until it is ridden
    let e = env st
    return (rested st { env = e { envVars = Map.insert name (FunValue (Function t ps body [] False)) (envVars e) } })
  Set t name [] e -> evaluate st e $ \v -> case checkType t v of
    Left err -> say st ("Error: " ++ err) >> return id
    Right _  -> return (\en -> en { envVars = Map.insert name v (envVars en) })
  Print e         -> evaluate st e $ \v -> do
    say st (show v)
    playCues (sounds st) (textCues (show v)) -- the answer is read out as well
    return id
  ReadStory e     -> do
    name <- newIORef Nothing
    st' <- evaluate st e $ \v -> writeIORef name (Just v) >> return id
    told <- readIORef name
    case told of
      Just (StrValue f) -> readStory st' f
      Just _            -> say st' ("Error: the name of a story must be a " ++ typeName TString) >> return st'
      Nothing           -> return st' -- the name could not be evaluated; already reported
  Lunge n body    -> playCues (sounds st) ["lunge"] >> rested <$> lunge st n body 1
  Treat n         -> care st [n] "treat" $ \h -> case moodOf n h of
    Just Sulking -> (clearMood n h, n ++ " munches the treat and decides to notice you again.")
    Just RanAway -> (h, n ++ " is not here to take the treat.")
    _            -> (h, n ++ " eats the treat anyway.")
  Interrogate n   -> care st [n] "interrogate" $ \h -> case moodOf n h of
    Just Lying   -> (clearMood n h, n ++ " cracks under questioning and promises to tell the truth.")
    Just RanAway -> (h, n ++ " is not here to be interrogated.")
    _            -> (h, n ++ " has nothing to confess.")
  Honey n         -> care st [n] "honey" $ \h -> case moodOf n h of
    Just RanAway -> (clearMood n h, n ++ " follows the honey back home.")
    _            -> (h, n ++ " licks the honey. It never left.")
  MakeUp a b      -> care st [a, b] "makeup" $ \h ->
    if feuding a b h then (endFeud a b h, a ++ " and " ++ b ++ " nuzzle and make up.")
                     else (h, a ++ " and " ++ b ++ " were not fighting.")

-- | Run every line of a story file, exactly as if it had been typed at the
-- prompt: one statement per line, backslash continuations, pssssst comments.
-- A story may read other stories, but not one that is already being read.
readStory :: ReplState -> FilePath -> IO ReplState
readStory st f
  | not (storyExtension `isSuffixOf` f) =
      say st ("Error: " ++ f ++ " is not a story (stories end in " ++ storyExtension ++ ")") >> return st
  | f `elem` telling st =
      say st ("Error: " ++ f ++ " is already being told") >> return st
  | otherwise = do
      r <- try (readFile f >>= \src -> length src `seq` return src) :: IO (Either IOException String)
      case r of
        Left _    -> say st ("Error: nobody knows the story " ++ f) >> return st
        Right src -> do
          st' <- runLines st { telling = f : telling st } src
          return st' { telling = telling st }

-- | How many laps a horse can be lunged for before it gets dizzy.
dizzyLaps :: Int
dizzyLaps = 100

-- | The loop. The horse is walked in circles until its number reaches zero,
-- and the commands run once per lap. The catches:
--
--   * the horse's number moves one step towards zero after every lap, so the
--     loop counts down (or up, from a negative number) and uses the horse up:
--     it says cl0p afterwards;
--   * the horse changes rein after every lap, so every second lap runs the
--     commands in reverse order;
--   * the horse is asked for its number before every lap, which is one more
--     chance for it to change mood. A sulking or runaway horse ends the loop
--     early (lunge it again after curing it to do the remaining laps); a lying
--     horse never admits to reaching zero;
--   * after 'dizzyLaps' laps the horse is too dizzy to go on, and bolts.
lunge :: ReplState -> Name -> [Command] -> Int -> IO ReplState
lunge st0 n body lap = case evalTop (env st) (Var n) of
    Left err           -> say st ("Error: " ++ err) >> return st
    Right (IntValue 0) -> playCues (sounds st) ["whoa"] >> return st
    Right (IntValue _)
      | lap > dizzyLaps -> do
          say st (n ++ " got dizzy after " ++ show dizzyLaps ++ " laps and bolted.")
          return st { env = (env st) { envHerd = setMood n RanAway (envHerd (env st)) } }
      | otherwise -> do
          st' <- foldM runCommand st (if even lap then reverse body else body)
          lunge st' { env = closer (env st') } n body (lap + 1)
    Right _            -> say st ("Error: only a " ++ typeName TInt ++ " can be lunged") >> return st
  where
    st = stir st0 (Var n)
    -- one step towards zero, if the commands left it a number at all
    closer en = case Map.lookup n (envVars en) of
      Just (IntValue k) -> en { envVars = Map.insert n (IntValue (k - signum k)) (envVars en) }
      _                 -> en

-- | One more line of concussion has passed.
rested :: ReplState -> ReplState
rested st = st { concussed = max 0 (concussed st - 1) }

-- | Look after some horses: change the herd and say what happened.
care :: ReplState -> [Name] -> String -> (Herd -> (Herd, String)) -> IO ReplState
care st names cue f = case filter (not . (`isHorse` e)) names of
  missing : _ -> say st ("Error: there is no horse called " ++ missing) >> return (rested st)
  []          -> do
    let (herd, msg) = f (envHerd e)
    playCues (sounds st) [cue]
    say st msg
    return (rested st { env = e { envHerd = herd } })
  where e = env st

-- | Evaluate an expression: let the horses it uses change mood, carry out any
-- electrocution effects, then hand the value to the continuation, which gives
-- back the change to make to the environment.
evaluate :: ReplState -> Expr -> (Value -> IO (Env -> Env)) -> IO ReplState
evaluate st0 e k = do
  playCues (sounds st0) (exprCues e)
  let st      = stir st0 e
      en      = env st
      result  = evalTop en e
      effects = either (const []) (const (electrocuteEffects en e)) (checkFeuds en e)
  mapM_ (say st . effectMessage) effects
  playCues (sounds st) (map effectCue effects)
  change <- case result of
    Left err             -> say st ("Error: " ++ err) >> return id
    Right (ErrValue err) -> say st ("Error: " ++ err) >> return id
    Right v              -> k v
  -- a fresh kick restarts the concussion for the lines *after* this one
  let left | any isKick effects = concussionLines
           | otherwise          = max 0 (concussed st - 1)
  return st { env = change (foldr bury en effects), concussed = left }
  where
    bury (HorseDied (Just n)) en = en { envVars = Map.delete n (envVars en), envHerd = forget n (envHerd en) }
    bury _ en                    = en
    isKick (KickedInHead _) = True
    isKick _                = False
    effectCue (KickedInHead _) = "kick"
    effectCue (HorseDied _)    = "death"

-- | Give every horse the expression uses a chance to change mood.
stir :: ReplState -> Expr -> ReplState
stir st e
  | calm st   = st
  | otherwise = st { env = en { envHerd = herd }, gen = g }
  where
    en        = env st
    used      = filter (`isHorse` en) (Set.toList (horsesIn en e))
    (herd, g) = stirUp used (envHerd en) (gen st)
