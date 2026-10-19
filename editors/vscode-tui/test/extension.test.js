'use strict';

// Loads extension.js with a fake vscode module and checks the wiring: the provider, the command,
// and a completion request that reaches the real helper.

const test = require('node:test');
const assert = require('node:assert/strict');
const fs = require('fs');
const path = require('path');
const Module = require('module');
const { racketAvailable, fakeVscode, makeDocument, SERVER, REPO_ROOT } = require('./support');

const skip = racketAvailable() ? false : 'racket is not on PATH';
const SRC = ['#lang tui', '(let ([names (list-of "ada" "grace")])', '  (Console.WriteLine names.Count))', ''].join('\n');

function loadExtension(vscode) {
  const realLoad = Module._load;
  Module._load = function (request, ...rest) {
    if (request === 'vscode') return vscode;
    return realLoad.call(this, request, ...rest);
  };
  try {
    delete require.cache[require.resolve('../extension')];
    return require('../extension');
  } finally {
    Module._load = realLoad;
  }
}

function fakeHost(overrides = {}) {
  const vscode = fakeVscode();
  const providers = [];
  const commands = {};
  const debugProviders = {};
  const config = { metaServerPath: SERVER, racketPath: 'racket', ...overrides };
  vscode.workspace = {
    workspaceFolders: [],
    getConfiguration: () => ({ get: (key, dflt) => (key in config ? config[key] : dflt) }),
    onDidCloseTextDocument: () => ({ dispose() {} }),
  };
  vscode.window.createOutputChannel = () => ({ append() {}, dispose() {} });
  vscode.languages = {
    registerCompletionItemProvider: (selector, provider, ...triggers) => {
      providers.push({ selector, provider, triggers });
      return { dispose() {} };
    },
  };
  vscode.commands = {
    registerCommand: (name, fn) => {
      commands[name] = fn;
      return { dispose() {} };
    },
  };
  vscode.debug = {
    registerDebugConfigurationProvider: (type, provider) => {
      debugProviders[type] = provider;
      return { dispose() {} };
    },
  };
  vscode.window.activeTextEditor = undefined;
  return { vscode, providers, commands, debugProviders };
}

test('activation registers the completion provider and the browse command', () => {
  const { vscode, providers, commands } = fakeHost();
  const ext = loadExtension(vscode);
  const context = { subscriptions: [] };
  ext.activate(context);
  assert.equal(providers.length, 1);
  assert.deepEqual(providers[0].selector, { language: 'racket' });
  assert.deepEqual(providers[0].triggers, ['.', ' ', '(']);
  assert.equal(typeof commands['tui.browse'], 'function');
  for (const s of context.subscriptions) if (s.dispose) s.dispose();
});

test('activation registers a debug configuration provider for the tui type', () => {
  const { vscode, debugProviders } = fakeHost();
  const ext = loadExtension(vscode);
  const context = { subscriptions: [] };
  ext.activate(context);
  assert.equal(typeof debugProviders.tui.resolveDebugConfigurationWithSubstitutedVariables, 'function');
  for (const s of context.subscriptions) if (s.dispose) s.dispose();
});

test('debugging without a Tui repository reports the setting and starts nothing', async () => {
  const { vscode, debugProviders } = fakeHost({ metaServerPath: '' });
  const ext = loadExtension(vscode);
  const context = { subscriptions: [] };
  ext.activate(context);
  const resolved = await debugProviders.tui.resolveDebugConfigurationWithSubstitutedVariables(
    undefined, { type: 'tui', request: 'launch', source: 'x.tui' });
  assert.equal(resolved, undefined);
  assert.equal(vscode.shown.error.length, 1);
  assert.match(vscode.shown.error[0], /tui.metaServerPath/);
  for (const s of context.subscriptions) if (s.dispose) s.dispose();
});

test('debugging with no source and no active file reports it and starts nothing', async () => {
  const { vscode, debugProviders } = fakeHost();
  const ext = loadExtension(vscode);
  const context = { subscriptions: [] };
  ext.activate(context);
  const resolved = await debugProviders.tui.resolveDebugConfigurationWithSubstitutedVariables(
    undefined, { type: 'tui', request: 'launch' });
  assert.equal(resolved, undefined);
  assert.match(vscode.shown.error[0], /open a .tui file/);
  for (const s of context.subscriptions) if (s.dispose) s.dispose();
});

test('debugging builds the source and returns a clr launch for its exe and PDB', { skip }, async () => {
  const { vscode, debugProviders } = fakeHost();
  const ext = loadExtension(vscode);
  const context = { subscriptions: [] };
  ext.activate(context);
  const source = path.join(REPO_ROOT, 'examples', 'basics.rkt');
  try {
    const config = await debugProviders.tui.resolveDebugConfigurationWithSubstitutedVariables(
      undefined, { type: 'tui', request: 'launch', name: 'Basics', source });
    assert.ok(config, `no launch configuration: ${JSON.stringify(vscode.shown)}`);
    assert.equal(config.type, 'clr');
    assert.equal(config.request, 'launch');
    assert.equal(config.name, 'Basics');
    assert.ok(fs.existsSync(config.program), 'the exe is written');
    assert.ok(fs.existsSync(config.program.replace(/\.exe$/, '.pdb')), 'the PDB is written beside it');
  } finally {
    for (const s of context.subscriptions) if (s.dispose) s.dispose();
  }
});

test('a completion request through the activated extension reaches the helper', { skip }, async () => {
  const { vscode, providers } = fakeHost();
  const ext = loadExtension(vscode);
  const context = { subscriptions: [] };
  ext.activate(context);
  try {
    const doc = makeDocument(SRC, 'wiring-test.tui', 1);
    const position = doc.positionAt(SRC.indexOf('(Console') + 2);
    const token = { isCancellationRequested: false };
    const items = await providers[0].provider.provideCompletionItems(doc, position, token);
    assert.ok(items.find((i) => i.label === 'names'), 'the visible value is offered');
  } finally {
    for (const s of context.subscriptions) if (s.dispose) s.dispose();
  }
});
