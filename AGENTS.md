# Guide for agents working on Tui

Tui is a Racket `#lang` that compiles to .NET IL, using [Rangi](https://github.com/pezipink/rangi)
for metadata, IL, and assembly. Read these before changing the compiler:

- [docs/architecture.md](docs/architecture.md): the pipeline, types, inference, overloads, module
  values, phase-1 API, REPL, and known limits.
- [docs/captures.md](docs/captures.md): closure captures, cells, and lambda classes.
- [README.md](README.md): the language reference.

Rangi is at `C:\repos\rangi`. Its IL forms are described in architecture.md section 11.

## Commands

Run from `C:\repos\tui`:

- Build after editing `main.rkt` or `private/*.rkt`: `raco make main.rkt`.
- Run an example: `racket examples\values.rkt`.
- Run the test suite: `raco test tests\tui-test.rkt`. It takes about 4 minutes, so run it async and
  read the output when it finishes.
- Verify an exported executable: `"C:\Program Files (x86)\Microsoft SDKs\Windows\v10.0A\bin\NETFX 4.6.1 Tools\PEVerify.exe" file.exe`.

## Pitfalls

- If you see struct mismatch errors, stale `compiled` directories are the usual cause. Delete the
  `compiled` directories, then run `raco make main.rkt`.
- When exporting an executable from a scratch file, require the runtime with an uppercase drive
  letter, `(file "C:/repos/tui/private/runtime.rkt")`. A lowercase `c:/` creates a second module
  instance, and the structs will not match.
- Run Racket files by path (`racket examples\x.rkt`). Inline `racket -e` with backslashes failed
  in PowerShell.
- Names with commas must use pipes, for example `|Func<int32, int32>|`.
- Property access is `.get_Count`. `.Count` is rejected.
- Delegates are called with `.Invoke`. Functions are called directly.
- Type inference only works when a generic parameter is exactly `(mvar n)`. Otherwise write the
  type arguments: `Enumerable.Where<int32>`.
- A macro that builds `(tui-il ...)` by hand must use `il`, or the type is missing and `tc` reports
  `internal: untyped fragment`.
- Expansion runs twice for methods that contain lambdas (architecture.md section 9). Macros must be
  deterministic.
- Edit `.rkt` files with the edit tool. CRLF line endings are normalised by git; don't rewrite them
  by hand.

## Conventions

- Only comment code that needs clarification.
- Keep the examples in `examples/` runnable, and add a test in `tests/tui-test.rkt` for each new
  feature.
- Update architecture.md when a compiler rule changes. Update README.md when the language changes.
