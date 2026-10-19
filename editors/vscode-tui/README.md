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
- **Debugging.** A `tui` launch configuration builds a `.tui` file to an exe with its PDB, then
  starts the exe under the C# extension's `clr` debugger. Breakpoints are set on statements in the
  `.tui` file.

Documents that are not `#lang tui` get nothing from this extension, so racket-langserver and
Magic Racket answer them as before.

## Setup

The extension runs `racket private/meta-server.rkt` from the Tui repository. It needs `racket` on
the path, and Tui's own dependencies installed.

Debugging needs the C# extension (`ms-dotnettools.csharp`), which the extension installs with it.
The program is a .NET Framework 4.x exe, so it runs on Windows.

Settings:

- `tui.metaServerPath`: the path to `private/meta-server.rkt`. If empty, the extension looks in the
  workspace folders for it. Debugging finds the build script (`build.rkt`) from the same path.
- `tui.racketPath`: the `racket` executable. Defaults to `racket`.

The helper starts the first time a Tui module asks for completion. Its output goes to the **Tui**
channel in the Output panel. Build output for debugging goes there too.

### Debugging a program

Open a `.tui` file, and choose **Debug Tui program** from the Run and Debug view (or add the
configuration from `launch.json`). The launch configuration takes:

- `source`: the `.tui` file to build. Defaults to the active editor (`${file}`).
- `args`: command-line arguments for the program.
- `console`: `internalConsole`, `integratedTerminal` or `externalTerminal`.
- `stopAtEntry`: stop on the first line of the program.

The exe and its PDB are written under the OS temp directory (`tui-debug`), one pair per source file.
Compile errors are shown in an error message, and the session does not start. The exe is a 64-bit
program, since the .NET Framework debugger only attaches to 64-bit processes. Parameters and locals
appear in the Variables view under their source names. Compiler temporaries appear without a name.


## Development

The extension is plain JavaScript with no build step, so it runs in VS Code's own Node without a
separate install. Its tests use Node's test runner. To run them with the Node that ships in VS Code:

```powershell
$env:ELECTRON_RUN_AS_NODE = 1
& "C:\path\to\Microsoft VS Code\Code.exe" --test test/syntax.test.js test/helper.test.js test/completion.test.js test/debug.test.js test/extension.test.js
```

With a Node installation, `npm test` runs the same files. The helper tests need `racket` on the path
and skip otherwise. The tests use a stand-in for the `vscode` module, so they do not exercise the
extension host itself.
