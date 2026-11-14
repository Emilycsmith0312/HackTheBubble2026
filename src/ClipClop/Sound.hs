-- | Sound effects.
--
-- A /cue/ is just a name, such as "clip" or "neigh". Playing a cue plays the
-- file @sounds/<cue>.wav@ (or .ogg, .flac, .mp3) if it exists and does nothing
-- otherwise, so:
--
--   * to give an existing cue a sound, drop a file with that name in @sounds/@;
--   * to add a new cue for a piece of syntax, add a line to 'cueOf';
--   * to add a new cue for something the REPL does, call 'playCues' there.
--
-- Sounds are played by whichever command-line player is installed (see
-- 'players'), one after another on a background thread, so the REPL never
-- waits for them. With no player or no sound files everything is silent.
--
-- Environment variables:
--
--   * @CLIPCLOP_SOUNDS@  the directory holding the sound files (default: sounds)
--   * @CLIPCLOP_PLAYER@  the command used to play a file, e.g. "mpv --no-video"
module ClipClop.Sound (Cue, Sounds, initSounds, playCues, exprCues) where

import ClipClop.BinaryExpressions (children)
import ClipClop.Value (Arm (..), Expr (..), Value (..))
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Exception (IOException, try)
import Control.Monad (filterM, forever, void)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe, listToMaybe)
import System.Directory (doesFileExist, findExecutable)
import System.Environment (lookupEnv)
import System.Process

type Cue = String

-- | A handle on the sound system: a queue of cues waiting to be played.
newtype Sounds = Sounds (Chan Cue)

-- CUES --

-- | The cues for an expression: its sub-expressions' cues, left to right, then its own.
exprCues :: Expr -> [Cue]
exprCues e = concatMap exprCues (children e) ++ cueOf e

-- | The cues for one piece of syntax. Add a line here to give something a sound.
cueOf :: Expr -> [Cue]
cueOf e = case e of
  Val v@(IntValue _)    -> clipClops (show v) -- cl1pcl0p plays clip, clop
  Val v@(FloatValue _)  -> clipClops (show v)
  Val (BoolValue True)  -> ["neigh", "neigh"] -- neighneigh
  Val (BoolValue False) -> ["neigh"]
  Electrocute _         -> ["electrocute"]
  Shout _               -> ["shout"]
  Whisper _             -> ["whisper"]
  Abs _                 -> ["geld"]
  Call _ _              -> ["giddyup"]
  Whinny _ arms         -> "whinny" : concat [ maybe "nicker" (const "snort") p : ["stomp" | stomp]
                                             | Arm p _ stomp <- arms ]
  _                     -> []

-- | One "clip" per cl1p and one "clop" per cl0p in a printed number.
clipClops :: String -> [Cue]
clipClops s
  | "cl1p" `isPrefixOf` s = "clip" : clipClops (drop 4 s)
  | "cl0p" `isPrefixOf` s = "clop" : clipClops (drop 4 s)
  | otherwise             = case s of
                              []       -> []
                              (_ : cs) -> clipClops cs

-- PLAYING --

-- | Command-line players to look for, best first, with the arguments each
-- needs to play one file quietly and then exit.
players :: [(String, [String])]
players =
  [ ("pw-play", [])                                                  -- PipeWire
  , ("paplay",  [])                                                  -- PulseAudio
  , ("aplay",   ["-q"])                                              -- ALSA (wav only)
  , ("afplay",  [])                                                  -- macOS
  , ("ffplay",  ["-nodisp", "-autoexit", "-loglevel", "quiet"])
  , ("mpv",     ["--no-video", "--really-quiet"])
  , ("play",    ["-q"])                                              -- SoX
  ]

-- | File extensions tried for each cue, in order.
extensions :: [String]
extensions = ["wav", "ogg", "flac", "mp3"]

-- | Start the sound system. Never fails: with no player it stays silent.
initSounds :: IO Sounds
initSounds = do
  dir    <- fromMaybe "sounds" <$> lookupEnv "CLIPCLOP_SOUNDS"
  player <- findPlayer
  queue  <- newChan
  _ <- forkIO . forever $ do
    cue <- readChan queue
    case player of
      Nothing          -> return ()
      Just (cmd, args) -> do
        file <- findSound dir cue
        maybe (return ()) (\f -> playFile cmd (args ++ [f])) file
  return (Sounds queue)

-- | Queue some cues to be played in order. Returns immediately.
playCues :: Sounds -> [Cue] -> IO ()
playCues (Sounds queue) = mapM_ (writeChan queue)

findPlayer :: IO (Maybe (FilePath, [String]))
findPlayer = do
  custom <- lookupEnv "CLIPCLOP_PLAYER"
  case words <$> custom of
    Just (cmd : args) -> return (Just (cmd, args))
    _ -> do
      found <- filterM (fmap (/= Nothing) . findExecutable . fst) players
      return (listToMaybe found)

findSound :: FilePath -> Cue -> IO (Maybe FilePath)
findSound dir cue =
  listToMaybe <$> filterM doesFileExist [dir ++ "/" ++ cue ++ "." ++ ext | ext <- extensions]

-- | Play one file and wait for it to finish. Failures are ignored.
playFile :: FilePath -> [String] -> IO ()
playFile cmd args = void (try run :: IO (Either IOException ()))
  where
    run = do
      (_, _, _, h) <- createProcess (proc cmd args)
                        { std_in = NoStream, std_out = NoStream, std_err = NoStream }
      void (waitForProcess h)
