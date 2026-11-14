-- | The interactive read-eval-print loop.
module ClipClop.REPL (repl) where

import ClipClop.BinaryExpressions
  (Command (..), Effect (..), Expr, Name, checkType, concussionLines, effectMessage, electrocuteEffects, eval, parseCommand)
import ClipClop.Value (Value (..))
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO

-- | Everything the REPL remembers between lines.
data ReplState = ReplState
  { vars      :: Map Name Value -- variables (horses)
  , concussed :: Int            -- lines left during which output prints backwards
  }

repl :: IO ()
repl = do
  hSetBuffering stdout NoBuffering
  putStrLn "Clip Clop REPL. Type :q to quit."
  loop (ReplState Map.empty 0)

-- | Read a line, run it, and loop with the updated state.
loop :: ReplState -> IO ()
loop st = do
  putStr "clipclop> "
  eof <- isEOF
  if eof
    then putStrLn ""
    else do
      line <- getLine
      case line of
        ":q" -> say st "Bye."
        _ | all (== ' ') line -> loop st
          | otherwise -> run st line >>= loop

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
