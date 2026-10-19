'use strict';

const test = require('node:test');
const assert = require('node:assert/strict');
const path = require('path');
const { exePathFor, buildArgs, sourceOf, clrConfig } = require('../lib/debug');

test('the exe goes in the given directory, named after the source', () => {
  const exe = exePathFor(path.join('C:', 'work', 'basics.tui'), path.join('C:', 'out'));
  assert.equal(path.dirname(exe), path.join('C:', 'out'));
  assert.match(path.basename(exe), /^basics-[0-9a-f]{8}\.exe$/);
});

test('two sources with one file name get different exes', () => {
  const a = exePathFor(path.join('C:', 'one', 'app.tui'), 'd');
  const b = exePathFor(path.join('C:', 'two', 'app.tui'), 'd');
  assert.notEqual(a, b);
});

test('the same source always gets the same exe', () => {
  const src = path.join('C:', 'work', 'app.tui');
  assert.equal(exePathFor(src, 'd'), exePathFor(src, 'd'));
});

test('build.rkt takes the source and the exe', () => {
  assert.deepEqual(buildArgs('build.rkt', 'app.tui', 'app.exe'), ['build.rkt', 'app.tui', 'app.exe']);
});

test('the source is the request attribute, or else the active file', () => {
  assert.equal(sourceOf({ source: 'a.tui' }, 'b.tui'), 'a.tui');
  assert.equal(sourceOf({}, 'b.tui'), 'b.tui');
  assert.equal(sourceOf({}, undefined), undefined);
});

test('the clr launch configuration runs the exe from its own directory', () => {
  const config = clrConfig(path.join('C:', 'out', 'app-1.exe'), { name: 'Run', args: ['x'] });
  assert.deepEqual(config, {
    name: 'Run',
    type: 'clr',
    request: 'launch',
    program: path.join('C:', 'out', 'app-1.exe'),
    cwd: path.join('C:', 'out'),
    args: ['x'],
    console: 'internalConsole',
    stopAtEntry: false,
  });
});

test('the clr launch configuration keeps the console and stop-at-entry choices', () => {
  const config = clrConfig('app.exe', { console: 'integratedTerminal', stopAtEntry: true });
  assert.equal(config.console, 'integratedTerminal');
  assert.equal(config.stopAtEntry, true);
  assert.equal(config.name, 'Tui program');
});
