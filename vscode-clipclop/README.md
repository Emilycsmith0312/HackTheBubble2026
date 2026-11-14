# Clip Clop for VS Code

Syntax highlighting, snippets and a run button for Clip Clop stories
(`.dotclipclop` files).

## Install

Build the package (needs only Python 3) and install it:

```
python3 package.py
code --install-extension clipclop-0.2.0.vsix
```

Then reload VS Code. To uninstall: `code --uninstall-extension hackthebubble.clipclop`.

## Running a story

Open a `.dotclipclop` file and do any of:

- press the ▶ button at the top right of the editor;
- press `Ctrl+Alt+H` (`Cmd+Alt+H` on a Mac);
- right-click in the editor, or on the file in the explorer, and choose **Run Story**;
- run **Clip Clop: Run Story** from the command palette.

The file is saved, then run in a terminal called "Clip Clop" with

```
cabal run -v0 bubble2026 -- /path/to/story.dotclipclop
```

from the cabal project's folder, so `cabal` and GHC must be installed. The
first run builds the project, which takes a while and prints nothing.

**Clip Clop: Open REPL** starts the interactive REPL in the same terminal.

### Finding the cabal project

The extension needs the folder that contains `bubble2026.cabal`. It looks, in
order, at:

1. the `clipclop.projectPath` setting, if set;
2. the folders open in the workspace;
3. the folders above the story.

So stories inside the repository just work. For stories anywhere else, either
open the repository folder in the same VS Code window or set
`clipclop.projectPath`.

Stories run from the project folder, so that the `sounds/` directory is found.
That means a relative path in `readStory "..."` is relative to the project
folder, not to the story.

## Settings

| Setting                | Default                         | Meaning |
|------------------------|---------------------------------|---------|
| `clipclop.projectPath` | (empty)                         | Folder containing `bubble2026.cabal`. |
| `clipclop.runCommand`  | `cabal run -v0 bubble2026 --`   | The command that runs Clip Clop; the story's path is added on the end. Put the path of a built `bubble2026` executable here to skip cabal. |
| `clipclop.calm`        | `false`                         | Run with `CLIPCLOP_CALM=1`, so the horses never change mood. |

## The file icon

`.dotclipclop` files get the icon in `icons/dotclipclop.svg`. To change it,
replace that file with your own SVG (same name, ideally square, about 32×32),
then rebuild and reinstall:

```
python3 package.py
code --install-extension clipclop-0.2.0.vsix --force
```

The icon appears in the explorer and on editor tabs. VS Code only shows
per-language icons when the active file icon theme has no icon of its own for
the file, which is the case for the default theme (Seti) and for Minimal.

## What is highlighted

Comments (`pssssst`), strings and characters, `cl1p`/`cl0p` numbers, `neigh` and
`neighneigh`, the type names, every keyword, operators, the name of the horse
in a `the TYPE NAME` declaration, riders, and the `\` that continues a line.

The keyword lists live in `syntaxes/clipclop.tmLanguage.json`. If a keyword is
renamed in `src/ClipClop/BinaryExpressions.hs`, rename it there too (and in
`snippets/clipclop.json`).

## Snippets

`the`, `ridden`, `foal`, `giddyup`, `whinny`, `lunge`, `readStory`, `herd`,
`stampede`, `cull`, `roundup`.

## Files

| File                                 | What it is |
|--------------------------------------|------------|
| `package.json`                       | the extension manifest: language, commands, menus, settings |
| `language-configuration.json`        | comment toggling and bracket pairs |
| `syntaxes/clipclop.tmLanguage.json`  | the grammar |
| `snippets/clipclop.json`             | the snippets |
| `icons/dotclipclop.svg`              | the icon for `.dotclipclop` files |
| `extension.js`                       | the Run Story and Open REPL commands (plain JavaScript, no build step) |
| `package.py`                         | builds the `.vsix` |
