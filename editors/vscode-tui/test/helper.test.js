'use strict';

// These tests run the real private/meta-server.rkt, so they need racket on PATH.

const test = require('node:test');
const assert = require('node:assert/strict');
const { racketAvailable, makeDocument, startClient } = require('./support');
const { documentState } = require('../lib/service');

const skip = racketAvailable() ? false : 'racket is not on PATH';
const FILE = 'helper-test.tui';
const SRC = [
  '#lang tui',
  '(let ([names (list-of "ada" "grace")])',
  '  (Console.WriteLine names.Count))',
  '',
].join('\n');

const { client, service } = startClient();
const doc = documentState(makeDocument(SRC, FILE, 1));
const at = (s) => SRC.indexOf(s);

test.after(() => client.dispose());

test('names at a body offset lists the visible value', { skip }, async () => {
  const names = await service.names(doc, at('(Console'));
  const entry = names.find((n) => n.name === 'names');
  assert.ok(entry, 'names is visible');
  assert.equal(entry.kind, 'value');
});

test('members of a visible value are its instance properties', { skip }, async () => {
  const members = await service.members(doc, at('(Console'), 'names');
  const count = members.find((m) => m.name === 'Count');
  assert.ok(count, 'Count is listed');
  assert.equal(count.kind, 'property');
  assert.match(count.type, /int32/);
});

test('members of a type name are its static members', { skip }, async () => {
  const members = await service.members(doc, at('(Console'), 'Console');
  assert.ok(members.find((m) => m.name === 'WriteLine' && m.kind === 'method'));
  assert.ok(members.find((m) => m.name === 'Out' && m.kind === 'property'));
});

test('types lists the public types of the references', { skip }, async () => {
  const types = await service.types(doc);
  const console = types.find((t) => t.name === 'Console' && t.namespace === 'System');
  assert.ok(console, 'System.Console is listed');
  assert.equal(typeof console.opened, 'boolean');
});

test('a failed expansion keeps the last good snapshot', { skip }, async () => {
  await service.names(doc, at('(Console'));
  await assert.rejects(client.scopes(FILE, '#lang tui\n(let ([x ('),
    (err) => err.reply && err.reply.ok === false && err.reply.kept === true);
  const names = await client.names(FILE, at('(Console'));
  assert.ok(names.names.find((n) => n.name === 'names'), 'the last good scopes still answer');
});
