-- | The interactive read-eval-print loop.
module ClipClop.REPL (repl) where

import ClipClop.BinaryExpressions (Command (..), Name, checkType, eval, parseCommand)
import ClipClop.Value (Value (..))
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO

repl :: IO ()
repl = do
  hSetBuffering stdout NoBuffering
  putStrLn "Clip Clop REPL. Type :q to quit."
  loop Map.empty

-- | Read a line, run it, and loop with the (possibly updated) variables.
loop :: Map Name Value -> IO ()
loop vars = do
  putStr "clipclop> "
  eof <- isEOF
  if eof
    then putStrLn ""
    else do
      line <- getLine
      case line of
        ":q" -> putStrLn "Bye."
        _ | all (== ' ') line -> loop vars
          | otherwise -> run line >>= loop
  where
    run line = case parseCommand line of
      Left err -> putStrLn ("Parse error: " ++ show err) >> return vars
      Right cmd -> execute vars cmd

-- | Run one command, returning the new variable environment.
execute :: Map Name Value -> Command -> IO (Map Name Value)
execute vars (Set t name e) = case eval vars e of
  Left err             -> putStrLn ("Error: " ++ err) >> return vars
  Right (ErrValue err) -> putStrLn ("Error: " ++ err) >> return vars
  Right v              -> case checkType t v of
    Left err -> putStrLn ("Error: " ++ err) >> return vars
    Right _  -> return (Map.insert name v vars)
execute vars (Print e) = do
  case eval vars e of
    Left err             -> putStrLn ("Error: " ++ err)
    Right (ErrValue err) -> putStrLn ("Error: " ++ err)
    Right v              -> print v
  return vars
