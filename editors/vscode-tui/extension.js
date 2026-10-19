'use strict';

const fs = require('fs');
const path = require('path');
const { execFile } = require('child_process');
const vscode = require('vscode');
const { MetaClient } = require('./lib/metaClient');
const { TuiService } = require('./lib/service');
const { createCompletionProvider } = require('./lib/completion');
const { browse } = require('./lib/browse');
const { exePathFor, buildArgs, sourceOf, clrConfig } = require('./lib/debug');

const HELPER = path.join('private', 'meta-server.rkt');

// The helper's path: the tui.metaServerPath setting, or private/meta-server.rkt in a workspace folder.
function findServer() {
  const configured = vscode.workspace.getConfiguration('tui').get('metaServerPath', '');
  if (configured) return configured;
  for (const folder of vscode.workspace.workspaceFolders || []) {
    const candidate = path.join(folder.uri.fsPath, HELPER);
    if (fs.existsSync(candidate)) return candidate;
  }
  return undefined;
}

// The Tui repository, found from the helper's path (private/meta-server.rkt sits two levels down).
function findRepo() {
  const server = findServer();
  return server ? path.dirname(path.dirname(server)) : undefined;
}

// Runs build.rkt to write the exe and its PDB. Its output goes to the log; a failure rejects with stderr.
function buildExe(racket, repo, source, exe, log) {
  return new Promise((resolve, reject) => {
    fs.mkdirSync(path.dirname(exe), { recursive: true });
    const args = buildArgs(path.join(repo, 'build.rkt'), source, exe);
    execFile(racket, args, { cwd: repo, windowsHide: true }, (err, stdout, stderr) => {
      log(stdout);
      log(stderr);
      if (err) reject(new Error((stderr || '').trim() || err.message));
      else resolve(exe);
    });
  });
}

function activate(context) {
  const output = vscode.window.createOutputChannel('Tui');
  const log = (text) => output.append(text);
  let client = null;
  let service = null;
  let reported = false;

  // The helper starts on first use. It is null when no helper can be found.
  function getService() {
    if (service) return service;
    const server = findServer();
    if (!server) {
      log('no metadata helper: set tui.metaServerPath to private/meta-server.rkt\n');
      if (!reported) {
        reported = true;
        vscode.window.showErrorMessage(
          'Tui: set tui.metaServerPath to private/meta-server.rkt in the Tui repository.');
      }
      return null;
    }
    const racket = vscode.workspace.getConfiguration('tui').get('racketPath', 'racket');
    client = new MetaClient({ racket, server, cwd: path.dirname(path.dirname(server)), log });
    service = new TuiService(client);
    return service;
  }

  const provider = createCompletionProvider(vscode, getService, log);
  context.subscriptions.push(
    vscode.languages.registerCompletionItemProvider({ language: 'racket' }, provider, '.', ' ', '('),
    vscode.commands.registerCommand('tui.browse', async () => {
      const editor = vscode.window.activeTextEditor;
      if (!editor) return;
      const svc = getService();
      if (!svc) return;
      try {
        await browse(vscode, svc, editor);
      } catch (err) {
        log(`browse failed: ${err.message}\n`);
        vscode.window.showErrorMessage(`Tui: ${err.message}`);
      }
    }),
    vscode.debug.registerDebugConfigurationProvider('tui', {
      // Runs after VS Code expands ${file} and other variables; the plain resolve hook runs before that.
      // Builds the source into an exe with its PDB, then hands the exe to the clr debugger.
      resolveDebugConfigurationWithSubstitutedVariables: async (_folder, request) => {
        const active = vscode.window.activeTextEditor;
        const source = sourceOf(request, active && active.document.uri.fsPath);
        if (!source) {
          vscode.window.showErrorMessage('Tui: open a .tui file to debug.');
          return undefined;
        }
        const repo = findRepo();
        if (!repo) {
          vscode.window.showErrorMessage(
            'Tui: set tui.metaServerPath to private/meta-server.rkt in the Tui repository.');
          return undefined;
        }
        const racket = vscode.workspace.getConfiguration('tui').get('racketPath', 'racket');
        const exe = exePathFor(source);
        try {
          await buildExe(racket, repo, source, exe, log);
        } catch (err) {
          log(`build failed: ${err.message}\n`);
          vscode.window.showErrorMessage(`Tui: build failed: ${err.message}`);
          return undefined;
        }
        return clrConfig(exe, request);
      },
    }),
    vscode.workspace.onDidCloseTextDocument((doc) => {
      if (service) service.forget(doc.uri.fsPath);
    }),
    { dispose: () => { if (client) client.dispose(); } },
    output);
}

function deactivate() {}

module.exports = { activate, deactivate };
