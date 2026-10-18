'use strict';

// Text helpers for the Tui editor companion. They do not use the vscode API, so they can be
// tested without an editor.

// The first line of a Tui module is `#lang tui`.
const TUI_LANG = /^#lang\s+tui(?:\s|$)/;

// The member access being typed at the end of a line prefix. The receiver starts at a token
// boundary, so `(System.Console.Wr` gives the receiver System.Console and the prefix Wr. A numeric
// literal can be a receiver too, as in `(1.ToStr`.
const MEMBER_ACCESS = /(?:^|[\s(\[])(\d+(?:\.\d+)?|[A-Za-z_]\w*(?:\.[A-Za-z_]\w*)*)\.((?:[A-Za-z_]\w*)?)$/;

function isTuiText(firstLine) {
  return TUI_LANG.test(firstLine);
}

function memberAccess(linePrefix) {
  const m = MEMBER_ACCESS.exec(linePrefix);
  return m ? { receiver: m[1], prefix: m[2] } : null;
}

function escapeSnippet(text) {
  return text.replace(/[$}\\]/g, '\\$&');
}

// The snippet browse inserts for a member. Methods take their arguments in head position, so the
// cursor lands inside the call. A method without parameters gets no cursor stop.
function memberInsertText(receiver, member) {
  const target = escapeSnippet(`${receiver}.${member.name}`);
  if (member.kind === 'property') return target;
  if (member.signature.startsWith('()')) return `(${target})`;
  return `(${target} $0)`;
}

module.exports = { isTuiText, memberAccess, memberInsertText };
