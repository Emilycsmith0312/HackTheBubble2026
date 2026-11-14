# Sound effects

Drop a file named `<cue>.wav` (or `.ogg`, `.flac`, `.mp3`) in this directory and
it plays whenever that cue happens. Missing files are simply skipped.

| Cue           | Plays when                                             |
|---------------|--------------------------------------------------------|
| `clip`        | each `cl1p` in a number that is evaluated              |
| `clop`        | each `cl0p` in a number that is evaluated              |
| `neigh`       | `neigh` (once) or `neighneigh` (twice) is evaluated    |
| `electrocute` | `electrocute ...`                                      |
| `shout`       | `speakupgirl ...`                                      |
| `whisper`     | `quietdowngirl ...`                                    |
| `geld`        | `geld ...`                                             |
| `giddyup`     | a function is ridden (`giddyup ... with ...`)          |
| `whinny`      | a `whinny` is evaluated                                |
| `snort`       | once for each `snort` in that whinny                   |
| `nicker`      | once for the `nicker` in that whinny                   |
| `stomp`       | once for each `stomp` in that whinny                   |
| `kick`        | you are kicked in the head                             |
| `death`       | a horse dies                                           |
| `treat`       | `treat NAME`                                           |
| `interrogate` | `interrogate NAME`                                     |
| `honey`       | `honey NAME`                                           |
| `makeup`      | `makeup NAME and NAME`                                 |

To add a cue for more syntax, add a line to `cueOf` in `src/ClipClop/Sound.hs`.
To add one for something the REPL does, call `playCues (sounds st) ["name"]` there.

Sounds are played with the first of `pw-play`, `paplay`, `aplay`, `afplay`,
`ffplay`, `mpv`, `play` that is installed (`aplay` only plays `.wav`).
Set `CLIPCLOP_PLAYER="some command"` to use a different player, and
`CLIPCLOP_SOUNDS=dir` to load the files from somewhere else.
