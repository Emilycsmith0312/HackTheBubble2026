-- | The interactive read-eval-print loop.
--
-- Line editing (arrow keys, history, Ctrl-R, emacs keys, tab completion of
-- commands) comes from haskeline. Enter runs the line. Pasting several lines
-- runs them one after another. Ctrl-C cancels the line, Ctrl-D quits.
module ClipClop.REPL (repl) where

import ClipClop.BinaryExpressions
  (Command (..), Effect (..), Expr, Name, checkType, concussionLines, effectMessage, electrocuteEffects, eval, parseCommand)
import ClipClop.Value (Value (..))
import Control.Exception (IOException, try)
import Control.Monad (foldM)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Data.Map (Map)
import qualified Data.Map as Map
import System.Console.Haskeline
import System.Directory (getHomeDirectory)
import System.IO

-- | Everything the REPL remembers between lines.
data ReplState = ReplState
  { vars      :: Map Name Value -- variables (horses)
  , concussed :: Int            -- lines left during which output prints backwards
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
  runInputT settings . withInterrupt $ do
    outputStrLn "Clip Clop REPL. Type :help for commands, :q to quit."
    loop (ReplState Map.empty 0)

------------------------------------------------------------------------------
-- The loop and the : commands

commandNames :: [String]
commandNames = [":help", ":vars", ":load", ":reset", ":quit", ":q"]

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
    Just (Just src) -> dispatch st src

-- | A line starting with ':' is a REPL command. Anything else is run as code,
-- one line at a time (so pasted text containing several lines still works).
dispatch :: ReplState -> String -> InputT IO ()
dispatch st src = case words src of
  []                              -> loop st
  (c : args) | ":" `isPrefixOf` c -> command st c args
  _                               -> liftIO (runLines st src) >>= loop

command :: ReplState -> String -> [String] -> InputT IO ()
command st c args = case (c, args) of
  (":q", _)      -> bye
  (":quit", _)   -> bye
  (":help", _)   -> outputStr helpText >> loop st
  (":vars", _)   -> outputStr (showVars st) >> loop st
  (":reset", _)  -> outputStrLn "All horses released." >> loop (ReplState Map.empty 0)
  (":load", [f]) -> loadFile st f >>= loop
  (":load", _)   -> outputStrLn "Usage: :load FILE" >> loop st
  _              -> outputStrLn ("Unknown command " ++ c ++ ". Try :help") >> loop st
  where bye = liftIO (say st "Bye.")

-- | Run every non-blank line of the text as its own statement.
runLines :: ReplState -> String -> IO ReplState
runLines st src = foldM run st (filter (not . all isSpace) (lines src))

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
  | Map.null (vars st) = "  (no horses yet)\n"
  | otherwise = unlines [ "  " ++ n ++ " = " ++ show v | (n, v) <- Map.toList (vars st) ]

helpText :: String
helpText = unlines
  [ "Commands"
  , "  :help        show this help"
  , "  :vars        list the horses (variables)"
  , "  :load FILE   run each line of FILE"
  , "  :reset       release all the horses"
  , "  :q           quit"
  , "Keys"
  , "  up/down history, Ctrl-R search, Tab completes commands,"
  , "  Ctrl-C cancels the line you are typing, Ctrl-D quits."
  , "  Pasting several lines runs them one after another."
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

-- | Run one line: parse it, carry out any electrocution effects, then run the command.
run :: ReplState -> String -> IO ReplState
run st line = do
  (vars', kicked) <- case parseCommand line of
    Left err  -> say st ("Parse error: " ++ show err) >> return (vars st, False)
    Right cmd -> do
      let effects = electrocuteEffects (vars st) (commandExpr cmd)
      mapM_ (say st . effectMessage) effects
      let survivors = foldr bury (vars st) effects
      newVars <- execute st survivors cmd
      return (newVars, any isKick effects)
  -- a fresh kick restarts the concussion for the lines *after* this one
  let left | kicked    = concussionLines
           | otherwise = max 0 (concussed st - 1)
  return st { vars = vars', concussed = left }
  where
    bury (HorseDied (Just n)) vs = Map.delete n vs
    bury _ vs                    = vs
    isKick (KickedInHead _) = True
    isKick _                = False

commandExpr :: Command -> Expr
commandExpr (Set _ _ e) = e
commandExpr (Print e)   = e

-- | Run one command, returning the new variable environment.
execute :: ReplState -> Map Name Value -> Command -> IO (Map Name Value)
execute st vs (Set t name e) = case eval (vars st) e of
  Left err             -> say st ("Error: " ++ err) >> return vs
  Right (ErrValue err) -> say st ("Error: " ++ err) >> return vs
  Right v              -> case checkType t v of
    Left err -> say st ("Error: " ++ err) >> return vs
    Right _  -> return (Map.insert name v vs)
execute st vs (Print e) = do
  case eval (vars st) e of
    Left err             -> say st ("Error: " ++ err)
    Right (ErrValue err) -> say st ("Error: " ++ err)
    Right v              -> say st (show v)
  return vs