'use strict';

// Loads extension.js with a fake vscode module and checks the wiring: the provider, the command,
// and a completion request that reaches the real helper.

const test = require('node:test');
const assert = require('node:assert/strict');
const Module = require('module');
const { racketAvailable, fakeVscode, makeDocument, SERVER } = require('./support');

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

function fakeHost() {
  const vscode = fakeVscode();
  const providers = [];
  const commands = {};
  const config = { metaServerPath: SERVER, racketPath: 'racket' };
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
  return { vscode, providers, commands };
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
