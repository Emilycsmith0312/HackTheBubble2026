# Sound effects

Drop a file named `<cue>.wav` (or `.ogg`, `.flac`, `.mp3`) in this directory and
it plays whenever that cue happens. Missing files are simply skipped.

| Cue           | Plays when                                             |
|---------------|--------------------------------------------------------|
| `clip`        | each `cl1p` in a number that is evaluated or printed   |
| `clop`        | each `cl0p` in a number that is evaluated or printed   |
| `neigh`       | each `neigh` evaluated or printed (`neighneigh` is two)|
| `electrocute` | `electrocute ...`                                      |
| `shout`       | `speakupgirl ...`                                      |
| `whisper`     | `quietdowngirl ...`                                    |
| `geld`        | `geld ...`                                             |
| `giddyup`     | a function is ridden (`giddyup ... with ...`)          |
| `stampede`    | `stampede ... through ...`                             |
| `cull`        | `cull ... from ...`                                    |
| `roundup`     | `roundup ... from ... through ...`                     |
| `whinny`      | a `whinny` is evaluated                                |
| `snort`       | once for each `snort` in that whinny                   |
| `nicker`      | once for the `nicker` in that whinny                   |
| `stomp`       | once for each `stomp` in that whinny                   |
| `lunge`       | a `lunge` loop starts                                  |
| `whoa`        | a `lunge` loop finishes all its laps                   |
| `kick`        | you are kicked in the head                             |
| `death`       | a horse dies                                           |
| `treat`       | `treat NAME`                                           |
| `interrogate` | `interrogate NAME`                                     |
| `honey`       | `honey NAME`                                           |
| `makeup`      | `makeup NAME and NAME`                                 |

A printed result is read out too: every `cl1p`, `cl0p` and `neigh` in it plays
its sound, even inside a string. To make another word do that, add it to
`soundWords` in `src/ClipClop/Sound.hs`.

To add a cue for more syntax, add a line to `cueOf` in `src/ClipClop/Sound.hs`.
To add one for something the REPL does, call `playCues (sounds st) ["name"]` there.

Each sound is offered to every installed player in turn (`pw-play`, `paplay`,
`afplay`, `gst-launch-1.0`, `ffplay`, `mpv`, `play`, then `aplay`) until one
plays it, so a file the first player cannot decode is still played by a later
one. `pw-play` and `paplay` cannot play MP3 on every system; `gst-launch-1.0`,
`ffplay` and `mpv` can. The file's contents decide, not its name: an MP3 that
has been renamed to `.wav` still works (`clip.wav` and `clop.wav` are these),
but it starts a little slower than a real WAV because the first player has to
fail first.

Set `CLIPCLOP_PLAYER="some command"` to use one particular player, and
`CLIPCLOP_SOUNDS=dir` to load the files from somewhere else.
