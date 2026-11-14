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
-- Sounds are played by the command-line players that are installed (see
-- 'players'), one after another on a background thread, so the REPL never
-- waits for them. If one player cannot play a file the next one is tried. With no player or no sound files everything is silent.
--
-- Environment variables:
--
--   * @CLIPCLOP_SOUNDS@  the directory holding the sound files (default: sounds)
--   * @CLIPCLOP_PLAYER@  the command used to play a file, e.g. "mpv --no-video"
module ClipClop.Sound (Cue, Sounds, initSounds, playCues, finishSounds, exprCues, textCues) where

import ClipClop.BinaryExpressions (children)
import ClipClop.Value (Arm (..), Expr (..), Value (..))
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Exception (IOException, try)
import Control.Monad (filterM, forever)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe, listToMaybe)
import System.Directory (doesFileExist, findExecutable)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.IO (IOMode (..), hGetChar, withBinaryFile)
import System.Process

type Cue = String

-- | A handle on the sound system: a queue of cues waiting to be played.
-- ('Left' is a request to be told when everything queued before it has played.)
newtype Sounds = Sounds (Chan (Either (MVar ()) Cue))

-- CUES --

-- | The cues for an expression: its sub-expressions' cues, left to right, then its own.
exprCues :: Expr -> [Cue]
exprCues e = concatMap exprCues (children e) ++ cueOf e

-- | The cues for one piece of syntax. Add a line here to give something a sound.
cueOf :: Expr -> [Cue]
cueOf e = case e of
  Val v@(IntValue _)    -> textCues (show v) -- cl1pcl0p plays clip, clop
  Val v@(FloatValue _)  -> textCues (show v)
  Val v@(BoolValue _)   -> textCues (show v) -- neigh once, neighneigh twice
  Electrocute _         -> ["electrocute"]
  Shout _               -> ["speakup"]
  Whisper _             -> ["quietdown"]
  Abs _                 -> ["geld"]
  Call _ _              -> ["giddyup"]
  Stampede _ _          -> ["stampede"]
  Cull _ _              -> ["cull"]
  Roundup _ _ _         -> ["roundup"]
  Whinny _ arms         -> "whinny" : concat [ maybe "nicker" (const "snort") p : ["stomp" | stomp]
                                             | Arm p _ stomp <- arms ]
  _                     -> []

-- | Words that make a sound wherever they turn up in text, and their cues.
-- Add a pair here to give another word a sound.
soundWords :: [(String, Cue)]
soundWords = [("cl1p", "clip"), ("cl0p", "clop"), ("neigh", "neigh")]

-- | The cues for a piece of text, such as a printed result: one for every
-- sound word in it, in order (so "neighneigh" is two neighs).
textCues :: String -> [Cue]
textCues [] = []
textCues s@(_ : rest) = case [ (w, cue) | (w, cue) <- soundWords, w `isPrefixOf` s ] of
  (w, cue) : _ -> cue : textCues (drop (length w) s)
  []           -> textCues rest

-- PLAYING --

-- | A command-line player: its name, the arguments that make it play one file
-- quietly and then exit, and whether it can only cope with real WAV data.
data Player = Player String (FilePath -> [String]) Bool

-- | The players to look for, best first. Each sound is offered to every
-- installed player in turn until one of them plays it successfully, so a file
-- the first player cannot decode (an MP3, say) still gets played by a later one.
players :: [Player]
players =
  [ Player "pw-play"        (\f -> [f]) False                       -- PipeWire (no MP3 on some systems)
  , Player "paplay"         (\f -> [f]) False                       -- PulseAudio (no MP3)
  , Player "afplay"         (\f -> [f]) False                       -- macOS
  , Player "gst-launch-1.0" (\f -> ["-q", "filesrc", "location=" ++ f, "!", "decodebin", "!", "audioconvert"
                                   , "!", "audioresample", "!", "autoaudiosink"]) False -- GStreamer (plays MP3)
  , Player "ffplay"         (\f -> ["-nodisp", "-autoexit", "-loglevel", "quiet", f]) False
  , Player "mpv"            (\f -> ["--no-video", "--really-quiet", f]) False
  , Player "play"           (\f -> ["-q", f]) False                 -- SoX
  , Player "aplay"          (\f -> ["-q", f]) True                  -- ALSA: plays anything else as noise
  ]

-- | File extensions tried for each cue, in order.
extensions :: [String]
extensions = ["wav", "ogg", "flac", "mp3"]

-- | Start the sound system. Never fails: with no player it stays silent.
initSounds :: IO Sounds
initSounds = do
  dir   <- fromMaybe "sounds" <$> lookupEnv "CLIPCLOP_SOUNDS"
  found <- findPlayers
  queue <- newChan
  _ <- forkIO . forever $ do
    next <- readChan queue
    case next of
      Left done -> putMVar done ()
      Right cue -> findSound dir cue >>= maybe (return ()) (playWith found)
  return (Sounds queue)

-- | Queue some cues to be played in order. Returns immediately.
playCues :: Sounds -> [Cue] -> IO ()
playCues (Sounds queue) = mapM_ (writeChan queue . Right)

-- | Wait until every cue queued so far has finished playing. Without this a
-- program that exits straight away takes its sounds with it.
finishSounds :: Sounds -> IO ()
finishSounds (Sounds queue) = do
  done <- newEmptyMVar
  writeChan queue (Left done)
  takeMVar done

-- | The installed players, or just the one named by CLIPCLOP_PLAYER.
findPlayers :: IO [Player]
findPlayers = do
  custom <- lookupEnv "CLIPCLOP_PLAYER"
  case words <$> custom of
    Just (cmd : args) -> return [Player cmd (\f -> args ++ [f]) False]
    _                 -> filterM (\(Player cmd _ _) -> (/= Nothing) <$> findExecutable cmd) players

findSound :: FilePath -> Cue -> IO (Maybe FilePath)
findSound dir cue =
  listToMaybe <$> filterM doesFileExist [dir ++ "/" ++ cue ++ "." ++ ext | ext <- extensions]

-- | Offer a file to each player in turn until one plays it.
playWith :: [Player] -> FilePath -> IO ()
playWith found file = do
  wav <- isWav file
  go [p | p@(Player _ _ wavOnly) <- found, wav || not wavOnly]
  where
    go [] = return ()
    go (Player cmd args _ : rest) = do
      ok <- playFile cmd (args file)
      if ok then return () else go rest

-- | Is this really WAV data? Goes by the contents, not the name: a file
-- called .wav is quite often an MP3.
isWav :: FilePath -> IO Bool
isWav file = either (const False) id <$> (try check :: IO (Either IOException Bool))
  where check = withBinaryFile file ReadMode $ \h -> do
          magic <- hGetChar h >>= \a -> hGetChar h >>= \b -> hGetChar h >>= \c -> hGetChar h >>= \d -> return [a, b, c, d]
          return (magic == "RIFF")

-- | Play one file and wait for it to finish. Says whether the player succeeded.
playFile :: FilePath -> [String] -> IO Bool
playFile cmd args = either (const False) id <$> (try run :: IO (Either IOException Bool))
  where
    run = do
      (_, _, _, h) <- createProcess (proc cmd args)
                        { std_in = NoStream, std_out = NoStream, std_err = NoStream }
      (== ExitSuccess) <$> waitForProcess h
