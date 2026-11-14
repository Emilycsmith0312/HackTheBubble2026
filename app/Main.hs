module Main (main) where

import ClipClop.REPL (repl, runStories)
import System.Environment (getArgs)

-- | With no arguments, start the REPL. With arguments, run each one as a
-- story file (as readStory would) and exit.
main :: IO ()
main = do
  files <- getArgs
  if null files then repl else runStories files
