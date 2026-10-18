'use strict';

const test = require('node:test');
const assert = require('node:assert/strict');
const { isTuiText, memberAccess, memberInsertText } = require('../lib/syntax');

test('a Tui module is recognised by its #lang line', () => {
  assert.equal(isTuiText('#lang tui'), true);
  assert.equal(isTuiText('#lang tui  ; comment'), true);
  assert.equal(isTuiText('#lang racket'), false);
  assert.equal(isTuiText('#lang tui/extra'), false);
  assert.equal(isTuiText('; #lang tui'), false);
});

test('member access is found at a token boundary', () => {
  assert.deepEqual(memberAccess('  (xs.'), { receiver: 'xs', prefix: '' });
  assert.deepEqual(memberAccess('(xs.Co'), { receiver: 'xs', prefix: 'Co' });
  assert.deepEqual(memberAccess('(Console.Wr'), { receiver: 'Console', prefix: 'Wr' });
  assert.deepEqual(memberAccess('(System.Console.'), { receiver: 'System.Console', prefix: '' });
  assert.deepEqual(memberAccess('[xs.Count'), { receiver: 'xs', prefix: 'Count' });
});

test('a numeric literal can be the receiver', () => {
  assert.deepEqual(memberAccess('(1.'), { receiver: '1', prefix: '' });
  assert.deepEqual(memberAccess('(f 2.x'), { receiver: '2', prefix: 'x' });
  assert.deepEqual(memberAccess('(1.5.ToSt'), { receiver: '1.5', prefix: 'ToSt' });
});

test('no member access where the dot is not after a name or a literal', () => {
  assert.equal(memberAccess('(define x 1.5'), null);
  assert.equal(memberAccess('(xs.Count)'), null);
  assert.equal(memberAccess('plain'), null);
});

test('browse inserts properties, fields and enum members bare, and methods as calls', () => {
  assert.equal(memberInsertText('xs', { name: 'Count', kind: 'property', type: 'int32' }), 'xs.Count');
  assert.equal(memberInsertText('System.Int32', { name: 'MaxValue', kind: 'field', type: 'int32' }),
    'System.Int32.MaxValue');
  assert.equal(memberInsertText('System.DayOfWeek', { name: 'Monday', kind: 'enum-member', type: 'DayOfWeek' }),
    'System.DayOfWeek.Monday');
  assert.equal(memberInsertText('xs', { name: 'Clear', kind: 'method', signature: '() -> void' }),
    '(xs.Clear)');
  assert.equal(memberInsertText('xs', { name: 'Add', kind: 'method', signature: '(string) -> void' }),
    '(xs.Add $0)');
  assert.equal(memberInsertText('System.Console', { name: 'WriteLine', kind: 'method', signature: '(string) -> void' }),
    '(System.Console.WriteLine $0)');
});

test('snippet special characters in names are escaped', () => {
  assert.equal(memberInsertText('x', { name: 'a$b}', kind: 'property', type: 'int32' }), 'x.a\\$b\\}');
});
