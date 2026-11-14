// Clip Clop for VS Code: runs .dotclipclop stories with the cabal project.
// Plain JavaScript with no dependencies, so there is no build step.
const vscode = require('vscode');
const fs = require('fs');
const path = require('path');

const CABAL_FILE = 'bubble2026.cabal';
const TERMINAL_NAME = 'Clip Clop';

/** Is this folder the Clip Clop cabal project? */
function isProject(dir) {
  return Boolean(dir) && fs.existsSync(path.join(dir, CABAL_FILE));
}

/**
 * Find the cabal project: the configured folder, else a workspace folder,
 * else the nearest folder above the story.
 */
function findProject(storyPath) {
  const configured = vscode.workspace.getConfiguration('clipclop').get('projectPath');
  if (configured) return isProject(configured) ? configured : undefined;

  for (const folder of vscode.workspace.workspaceFolders || []) {
    if (isProject(folder.uri.fsPath)) return folder.uri.fsPath;
  }
  let dir = storyPath ? path.dirname(storyPath) : undefined;
  while (dir) {
    if (isProject(dir)) return dir;
    const parent = path.dirname(dir);
    if (parent === dir) break;
    dir = parent;
  }
  return undefined;
}

/** Quote a path for the shell the terminal is running. */
function quote(p) {
  return process.platform === 'win32' ? `"${p}"` : `'${p.replace(/'/g, `'\\''`)}'`;
}

/** The terminal for this project, created if needed (a new one if the settings changed). */
function terminalFor(project) {
  const calm = vscode.workspace.getConfiguration('clipclop').get('calm');
  const name = calm ? `${TERMINAL_NAME} (calm)` : TERMINAL_NAME;
  const existing = vscode.window.terminals.find(
    (t) => t.name === name && t.creationOptions && t.creationOptions.cwd === project && t.exitStatus === undefined
  );
  if (existing) return existing;
  return vscode.window.createTerminal({ name, cwd: project, env: calm ? { CLIPCLOP_CALM: '1' } : {} });
}

async function noProject() {
  const choice = await vscode.window.showErrorMessage(
    `Clip Clop: could not find the cabal project (a folder containing ${CABAL_FILE}). ` +
      'Open that folder in VS Code, or set "clipclop.projectPath".',
    'Open Settings'
  );
  if (choice) vscode.commands.executeCommand('workbench.action.openSettings', 'clipclop.projectPath');
}

/** Run a story: the file given by the explorer menu, or the one in the active editor. */
async function runStory(uri) {
  const editor = vscode.window.activeTextEditor;
  const target = uri || (editor && editor.document.uri);
  if (!target || target.scheme !== 'file') {
    vscode.window.showErrorMessage('Clip Clop: open a saved .dotclipclop file to run it.');
    return;
  }
  const story = target.fsPath;
  if (!story.endsWith('.dotclipclop')) {
    vscode.window.showErrorMessage('Clip Clop: that is not a story (stories end in .dotclipclop).');
    return;
  }
  const open = vscode.workspace.textDocuments.find((d) => d.uri.fsPath === story);
  if (open && open.isDirty) await open.save();

  const project = findProject(story);
  if (!project) return noProject();

  const command = vscode.workspace.getConfiguration('clipclop').get('runCommand');
  const terminal = terminalFor(project);
  terminal.show(true);
  terminal.sendText(`${command} ${quote(story)}`);
}

/** Open the interactive REPL in the project folder. */
async function openRepl() {
  const editor = vscode.window.activeTextEditor;
  const project = findProject(editor && editor.document.uri.scheme === 'file' ? editor.document.uri.fsPath : undefined);
  if (!project) return noProject();

  const command = vscode.workspace.getConfiguration('clipclop').get('runCommand');
  const terminal = terminalFor(project);
  terminal.show(false);
  terminal.sendText(command.replace(/\s*--\s*$/, ''));
}

function activate(context) {
  context.subscriptions.push(
    vscode.commands.registerCommand('clipclop.runStory', runStory),
    vscode.commands.registerCommand('clipclop.openRepl', openRepl)
  );
}

function deactivate() {}

module.exports = { activate, deactivate, findProject, quote };
