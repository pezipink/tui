'use strict';

const fs = require('fs');
const path = require('path');
const vscode = require('vscode');
const { MetaClient } = require('./lib/metaClient');
const { TuiService } = require('./lib/service');
const { createCompletionProvider } = require('./lib/completion');
const { browse } = require('./lib/browse');

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
    vscode.workspace.onDidCloseTextDocument((doc) => {
      if (service) service.forget(doc.uri.fsPath);
    }),
    { dispose: () => { if (client) client.dispose(); } },
    output);
}

function deactivate() {}

module.exports = { activate, deactivate };
