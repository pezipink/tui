'use strict';

const { isTuiText, memberAccess } = require('./syntax');
const { documentState } = require('./service');

// Items from this companion sort ahead of racket-langserver's, which sorts by name alone.
const RANK = '0';
const RANK_TYPE = '1';

// The signature sits in the label, not in the item's detail, so every overload of a method shows its
// parameter types in the list rather than only the focused one.
const MEMBER_KINDS = {
  property: 'Property',
  field: 'Field',
  'enum-member': 'EnumMember',
  method: 'Method',
};

function memberItem(vscode, member, range) {
  const typed = member.kind !== 'method';
  const label = typed
    ? { label: member.name, description: member.type }
    : { label: member.name, detail: member.signature };
  const item = new vscode.CompletionItem(
    label,
    vscode.CompletionItemKind[MEMBER_KINDS[member.kind] || 'Method']);
  item.range = range;
  item.sortText = RANK + member.name;
  return item;
}

function nameItems(vscode, names, types) {
  const items = names.map((n) => {
    const kind = n.kind === 'function' ? vscode.CompletionItemKind.Function
      : vscode.CompletionItemKind.Variable;
    const item = new vscode.CompletionItem(n.name, kind);
    item.detail = n.type;
    item.sortText = RANK + n.name;
    return item;
  });
  // Only opened types with no type arguments can be written by name alone.
  for (const t of types) {
    if (!t.opened || t.arity !== 0) continue;
    const item = new vscode.CompletionItem(t.name, vscode.CompletionItemKind.Class);
    item.detail = t.namespace;
    item.sortText = RANK_TYPE + t.name;
    items.push(item);
  }
  return items;
}

// The provider for `racket` documents. Documents that are not Tui modules get nothing, so
// racket-langserver and Magic Racket are left to answer them.
function createCompletionProvider(vscode, getService, log) {
  return {
    async provideCompletionItems(document, position, token) {
      if (!isTuiText(document.lineAt(0).text)) return undefined;
      const service = getService();
      if (!service) return undefined;
      try {
        const doc = documentState(document);
        const offset = document.offsetAt(position);
        const linePrefix = document.lineAt(position.line).text.slice(0, position.character);
        const access = memberAccess(linePrefix);
        if (access) {
          const members = await service.members(doc, offset, access.receiver);
          if (token.isCancellationRequested) return [];
          const range = new vscode.Range(position.line, position.character - access.prefix.length,
            position.line, position.character);
          return members.map((m) => memberItem(vscode, m, range));
        }
        const [names, types] = await Promise.all([service.names(doc, offset), service.types(doc)]);
        if (token.isCancellationRequested) return [];
        return nameItems(vscode, names, types);
      } catch (err) {
        log(`completion failed: ${err.message}\n`);
        return undefined;
      }
    },
  };
}

module.exports = { createCompletionProvider, memberItem, nameItems };
