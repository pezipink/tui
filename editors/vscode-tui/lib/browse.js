'use strict';

const { isTuiText, memberInsertText } = require('./syntax');
const { documentState } = require('./service');

// Two QuickPicks: a visible value or a type, then one of its members. The member is inserted at the
// cursor in the dotted form the compiler reads, such as (xs.Add $0) or System.Console.Out.
// Generic types are left out, since their type arguments cannot be chosen here.
async function browse(vscode, service, editor) {
  const document = editor.document;
  if (!isTuiText(document.lineAt(0).text)) {
    vscode.window.showInformationMessage('Tui: Browse Members works in #lang tui modules.');
    return;
  }
  const doc = documentState(document);
  const offset = document.offsetAt(editor.selection.active);
  const [names, types] = await Promise.all([service.names(doc, offset), service.types(doc)]);

  const valuePicks = names
    .filter((n) => n.kind === 'value')
    .map((n) => ({ label: `$(symbol-variable) ${n.name}`, description: n.type, receiver: n.name }));
  const typePicks = types
    .filter((t) => t.arity === 0)
    .sort((a, b) => (b.opened - a.opened) || a.name.localeCompare(b.name))
    .map((t) => ({
      label: `$(symbol-class) ${t.name}`,
      description: t.namespace,
      receiver: t.opened ? t.name : `${t.namespace}.${t.name}`,
    }));

  const pick = await vscode.window.showQuickPick([...valuePicks, ...typePicks], {
    title: 'Tui: Browse Members',
    placeHolder: 'Choose a value or a type',
    matchOnDescription: true,
  });
  if (!pick) return;

  const members = await service.members(doc, offset, pick.receiver);
  if (members.length === 0) {
    vscode.window.showInformationMessage(`Tui: no public members of ${pick.receiver}.`);
    return;
  }
  const memberPick = await vscode.window.showQuickPick(
    members.map((m) => ({
      label: m.name,
      description: m.kind === 'property' ? m.type : m.signature,
      detail: m.kind,
      member: m,
    })),
    { title: `Tui: Members of ${pick.receiver}`, matchOnDescription: true });
  if (!memberPick) return;

  const text = memberInsertText(pick.receiver, memberPick.member);
  await editor.insertSnippet(new vscode.SnippetString(text), editor.selection);
}

module.exports = { browse };
