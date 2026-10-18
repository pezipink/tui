# Tui metadata for VS Code

Completion and member browsing for Tui modules (`#lang tui`). The names and members come from the
compiler's own expansion and from CLR metadata, through the editor helper
[private/meta-server.rkt](/c:/repos/tui/private/meta-server.rkt).

## What it does

- **Name completion.** Inside a Tui module, completion offers the values and functions in scope at
  the cursor, and the opened types that take no type arguments. These sort ahead of
  racket-langserver's names.
- **Member completion.** After a receiver and a dot, such as `names.Co`, it offers the receiver's
  instance members, including those inherited from base classes such as `ToString` from `Object`.
  A numeric literal works as a receiver too, so `1.` offers `ToString` and the other `Int32`
  instance members. Strings and chars do not have a dotted form, so `"x".` gets no completion.
  For a type name such as `Console.`, it offers the static members. Properties are
  listed by name with their type. Each overload of a method is its own entry, with its parameter
  types shown in the list, such as `WriteLine(string) -> void`.
- **Tui: Browse Members.** A command that asks for a value or type, then for one of its members, and
  inserts the member at the cursor. Methods with arguments insert a call with a snippet stop for the
  first argument.

Documents that are not `#lang tui` get nothing from this extension, so racket-langserver and
Magic Racket answer them as before.

## Setup

The extension runs `racket private/meta-server.rkt` from the Tui repository. It needs `racket` on
the path, and Tui's own dependencies installed.

Settings:

- `tui.metaServerPath`: the path to `private/meta-server.rkt`. If empty, the extension looks in the
  workspace folders for it.
- `tui.racketPath`: the `racket` executable. Defaults to `racket`.

The helper starts the first time a Tui module asks for completion. Its output goes to the **Tui**
channel in the Output panel.

## Development

The extension is plain JavaScript with no build step, so it runs in VS Code's own Node without a
separate install. Its tests use Node's test runner. To run them with the Node that ships in VS Code:

```powershell
$env:ELECTRON_RUN_AS_NODE = 1
& "C:\path\to\Microsoft VS Code\Code.exe" --test test/syntax.test.js test/helper.test.js test/completion.test.js test/extension.test.js
```

With a Node installation, `npm test` runs the same files. The helper tests need `racket` on the path
and skip otherwise. The tests use a stand-in for the `vscode` module, so they do not exercise the
extension host itself.
