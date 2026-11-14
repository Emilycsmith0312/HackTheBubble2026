-- | The interactive read-eval-print loop.
module ClipClop.REPL (repl) where

import ClipClop.Eval (eval)
import ClipClop.Parser (parseExpr)
import System.IO

repl :: IO ()
repl = do
  hSetBuffering stdout NoBuffering
  putStrLn "Clip Clop REPL. Type :q to quit."
  loop
  where
    loop = do
      putStr "clipclop> "
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
