'use strict';

// Test support: a stand-in for the vscode module, a document built from text, and a metadata
// client that runs the real private/meta-server.rkt.

const path = require('path');
const { spawnSync } = require('child_process');
const { MetaClient } = require('../lib/metaClient');
const { TuiService } = require('../lib/service');

const REPO_ROOT = path.resolve(__dirname, '..', '..', '..');
const SERVER = path.join(REPO_ROOT, 'private', 'meta-server.rkt');
const RACKET = process.env.TUI_RACKET || 'racket';

function racketAvailable() {
  const r = spawnSync(RACKET, ['--version'], { windowsHide: true });
  return r.status === 0;
}

class Range {
  constructor(startLine, startChar, endLine, endChar) {
    this.start = { line: startLine, character: startChar };
    this.end = { line: endLine, character: endChar };
  }
}

class CompletionItem {
  constructor(label, kind) {
    this.label = label;
    this.kind = kind;
  }
}

// The text of an item's label, which may be a plain string or a CompletionItemLabel.
function labelOf(item) {
  return typeof item.label === 'string' ? item.label : item.label.label;
}

class SnippetString {
  constructor(value) {
    this.value = value;
  }
}

const CompletionItemKind = {
  Function: 'Function', Variable: 'Variable', Class: 'Class', Property: 'Property', Method: 'Method',
  Field: 'Field', EnumMember: 'EnumMember',
};

function fakeVscode() {
  const shown = { info: [], error: [] };
  return {
    shown,
    Range,
    CompletionItem,
    CompletionItemKind,
    SnippetString,
    window: {
      showQuickPick: async () => undefined,
      showInformationMessage: (m) => shown.info.push(m),
      showErrorMessage: (m) => shown.error.push(m),
    },
  };
}

// A document with the parts of the vscode TextDocument the companion uses.
function makeDocument(text, file, version = 1) {
  const lines = text.split('\n');
  const starts = [];
  let offset = 0;
  for (const line of lines) {
    starts.push(offset);
    offset += line.length + 1;
  }
  return {
    uri: { fsPath: file },
    version,
    getText: () => text,
    lineAt: (i) => ({ text: lines[i] }),
    offsetAt: (p) => starts[p.line] + p.character,
    positionAt: (o) => {
      let line = 0;
      while (line + 1 < starts.length && starts[line + 1] <= o) line++;
      return { line, character: o - starts[line] };
    },
  };
}

function startClient() {
  const client = new MetaClient({ racket: RACKET, server: SERVER, cwd: REPO_ROOT, timeoutMs: 60000 });
  return { client, service: new TuiService(client) };
}

module.exports = { SERVER, racketAvailable, fakeVscode, makeDocument, startClient, labelOf };
