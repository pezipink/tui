'use strict';

const test = require('node:test');
const assert = require('node:assert/strict');
const { racketAvailable, fakeVscode, makeDocument, startClient, labelOf } = require('./support');
const { createCompletionProvider, memberItem } = require('../lib/completion');
const { browse } = require('../lib/browse');

const skip = racketAvailable() ? false : 'racket is not on PATH';
const FILE = 'completion-test.tui';
const SRC = [
  '#lang tui',
  '(let ([names (list-of "ada" "grace")])',
  '  (Console.WriteLine names.Count))',
  '',
].join('\n');

const token = { isCancellationRequested: false };
const { client, service } = startClient();
test.after(() => client.dispose());

function positionAfter(doc, text, extra = 0) {
  const offset = SRC.indexOf(text) + text.length + extra;
  return doc.positionAt(offset);
}

test('fields and enum members get their own item kinds, typed like properties', () => {
  const vscode = fakeVscode();
  const range = {};
  const field = memberItem(vscode, { name: 'MaxValue', kind: 'field', type: 'int32' }, range);
  assert.equal(field.kind, vscode.CompletionItemKind.Field);
  assert.deepEqual(field.label, { label: 'MaxValue', description: 'int32' });
  const monday = memberItem(vscode, { name: 'Monday', kind: 'enum-member', type: 'DayOfWeek' }, range);
  assert.equal(monday.kind, vscode.CompletionItemKind.EnumMember);
  assert.deepEqual(monday.label, { label: 'Monday', description: 'DayOfWeek' });
});

test('a document that is not a Tui module gets no completions', async () => {
  const vscode = fakeVscode();
  const provider = createCompletionProvider(vscode, () => service, () => {});
  const doc = makeDocument('#lang racket\n(displa', 'other.rkt', 1);
  const items = await provider.provideCompletionItems(doc, doc.positionAt(20), token);
  assert.equal(items, undefined);
});

test('name completion offers visible values', { skip }, async () => {
  const vscode = fakeVscode();
  const provider = createCompletionProvider(vscode, () => service, () => {});
  const doc = makeDocument(SRC, FILE, 1);
  const items = await provider.provideCompletionItems(doc, positionAfter(doc, '(Console'), token);
  const names = items.find((i) => i.label === 'names');
  assert.ok(names, 'names is offered');
  assert.equal(names.kind, vscode.CompletionItemKind.Variable);
  assert.equal(names.sortText[0], '0', 'companion items rank first');
});

test('member completion offers the receiver members with the typed prefix as range', { skip }, async () => {
  const vscode = fakeVscode();
  const provider = createCompletionProvider(vscode, () => service, () => {});
  const doc = makeDocument(SRC, FILE, 1);
  const position = positionAfter(doc, 'names.Count', -3); // after "names.Co"
  const items = await provider.provideCompletionItems(doc, position, token);
  const count = items.find((i) => labelOf(i) === 'Count');
  assert.ok(count, 'Count is offered');
  assert.equal(count.kind, vscode.CompletionItemKind.Property);
  assert.equal(count.range.start.character, position.character - 2, 'the range covers the typed "Co"');
  assert.equal(count.range.end.character, position.character);
});

test('every overload of a method shows its signature in the label', { skip }, async () => {
  const vscode = fakeVscode();
  const provider = createCompletionProvider(vscode, () => service, () => {});
  const doc = makeDocument(SRC, FILE, 1);
  const position = positionAfter(doc, 'Console.WriteLine', -'WriteLine'.length); // after "Console."
  const items = await provider.provideCompletionItems(doc, position, token);
  const overloads = items.filter((i) => labelOf(i) === 'WriteLine');
  assert.ok(overloads.length > 1, 'Console.WriteLine has several overloads');
  for (const o of overloads) {
    assert.match(o.label.detail, /^\(.*\) -> void$/, 'the parameter types are in the label');
    assert.equal(o.detail, undefined, 'the signature is not only in the detail');
  }
  assert.ok(overloads.some((o) => o.label.detail === '(string) -> void'));
});

test('a cancelled request returns nothing', { skip }, async () => {

  const vscode = fakeVscode();
  const provider = createCompletionProvider(vscode, () => service, () => {});
  const doc = makeDocument(SRC, FILE, 1);
  const cancelled = { isCancellationRequested: true };
  const items = await provider.provideCompletionItems(doc, positionAfter(doc, '(Console'), cancelled);
  assert.deepEqual(items, []);
});

test('a failing helper is logged and gives no completions', async () => {
  const vscode = fakeVscode();
  const logged = [];
  const failing = {
    names: async () => { throw new Error('boom'); },
    types: async () => [],
    members: async () => { throw new Error('boom'); },
  };
  const provider = createCompletionProvider(vscode, () => failing, (t) => logged.push(t));
  const doc = makeDocument(SRC, FILE, 1);
  const items = await provider.provideCompletionItems(doc, positionAfter(doc, '(Console'), token);
  assert.equal(items, undefined);
  assert.match(logged.join(''), /completion failed: boom/);
});

test('with no helper there are no completions', async () => {
  const vscode = fakeVscode();
  const provider = createCompletionProvider(vscode, () => null, () => {});
  const doc = makeDocument(SRC, FILE, 1);
  assert.equal(await provider.provideCompletionItems(doc, positionAfter(doc, '(Console'), token), undefined);
});

test('browse inserts the chosen member in dotted form', { skip }, async () => {
  const vscode = fakeVscode();
  const doc = makeDocument(SRC, FILE, 1);
  const inserted = [];
  const editor = {
    document: doc,
    selection: { active: positionAfter(doc, '(Console') },
    insertSnippet: async (snippet) => inserted.push(snippet.value),
  };
  const picks = [];
  vscode.window.showQuickPick = async (items) => {
    picks.push(items.length);
    if (picks.length === 1) return items.find((i) => i.receiver === 'names');
    return items.find((i) => i.label === 'Count');
  };
  await browse(vscode, service, editor);
  assert.deepEqual(inserted, ['names.Count']);
  assert.equal(picks.length, 2);
});

test('browse does nothing when the first pick is dismissed', { skip }, async () => {
  const vscode = fakeVscode();
  const doc = makeDocument(SRC, FILE, 1);
  const inserted = [];
  const editor = {
    document: doc,
    selection: { active: positionAfter(doc, '(Console') },
    insertSnippet: async (snippet) => inserted.push(snippet.value),
  };
  vscode.window.showQuickPick = async () => undefined;
  await browse(vscode, service, editor);
  assert.deepEqual(inserted, []);
});
