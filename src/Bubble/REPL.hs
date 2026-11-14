-- | The interactive read-eval-print loop.
module Bubble.REPL (repl) where

import Bubble.Eval (eval)
import Bubble.Parser (parseExpr)
import System.IO

repl :: IO ()
repl = do
  hSetBuffering stdout NoBuffering
  putStrLn "Bubble REPL. Type :q to quit."
  loop
  where
    loop = do
      putStr "bubble> "
      eof <- isEOF
      if eof
        then putStrLn ""
        else do
          line <- getLine
          case line of
            ":q" -> putStrLn "Bye."
            ""   -> loop
            _    -> do
              either print (print . eval) (parseExpr line)
              loop
