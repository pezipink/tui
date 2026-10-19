# Tui architecture

This is the reference for how the Tui compiler works. It is for people changing the compiler and
for agents working in this repository. For the language itself, see the [README](../README.md).
Closure captures have their own design notes in [captures.md](captures.md); this document
summarises them and points there for detail.

## 1. What Tui is

Tui is a Racket `#lang` that compiles to .NET IL. The macro expander is the compiler. Each Tui
form is a Racket macro that type checks its subforms by expanding them, and produces a typed
fragment of Rangi ilasm. Output is a PE executable (`.exe`) written by Rangi.

Because Tui forms are Racket macros, hygiene, `define-syntax-rule` and `define-syntax-parser` work
without special support. Local variables are real Racket bindings in internal definition contexts.

Rangi is a separate project (`c:\repos\rangi`). It reads .NET metadata, lowers ilasm to bytes, and
assembles the executable. Tui depends on it for metadata reading, IL text, and assembly. See
section 11 for the exact IL forms.

## 2. Files

| File | Contents |
| --- | --- |
| `main.rkt` | The language: every Tui form, the type checker `tc`, function and lambda compilation, module and REPL expansion, phase-1 helpers and syntax classes. |
| `private/clr.rkt` | The CLR bridge: reference configuration, type datums, metadata lookup, `coercion`, overload and generic-method resolution, and the phase-0 declarations of records. |
| `private/runtime.rkt` | Program assembly: `make-tui-program`, `tui-program->il`, `tui-write-exe`, `run-tui-program`. |
| `private/meta-server.rkt` | The editor helper: JSON lines on stdio for scopes, names, types and members. See section 4. |
| `private/type-patterns.rkt` | Type-pattern runtime (phase 0): the solver, the named-pattern registry and `match-type-pattern`. See section 12.1. |
| `private/type-syntax-class.rkt` | Type-pattern compiler: `define-type-syntax-class`, `type-match` and `type-matches?`. See section 12.1. |
| `tests/tui-test.rkt` | The test suite (79 tests, about 4 minutes). |
| `tests/meta-server-test.rkt` | Tests for the editor helper's requests, in process. |
| `editors/vscode-tui/` | The VS Code companion: completion and Browse Members, through the helper. See section 4. |
| `examples/` | Runnable programs. Each one is covered by a test. See section 12. |
| `docs/captures.md` | The design notes for closure captures. |

## 3. Pipeline

For a file `m.rkt` written in `#lang tui`:

1. **Reader.** Tui uses the standard Racket reader, so source is s-expressions.
2. **`tui-module-begin`** (installed as `#%module-begin`) receives every top-level form. It
   classifies each form with `form-kind`:
   - `using` and `open` change the CLR configuration.
   - `define` declares a function.
   - `value` is a module-level value define (section 8).
   - `passthrough` is a Racket macro definition, a phase-1 definition (`define-syntax-class`, `define-splicing-syntax-class`, `define-simple-macro`, `begin-for-syntax`, `define-for-syntax`, `define-syntaxes`) or `require`/`provide`, emitted unchanged.
   - `expr` is a top-level expression, collected into the entry point.
3. **Configuration.** `tui-configure!` is called with the references (mscorlib plus each `using`)
   and the open namespaces (`System` plus each `open`). It is called again inside the emitted
   module body. Visiting a required Tui module resets the shared state, so the configuration must
   be set after the `pass` forms.
4. **Expansion.** Function bodies and the entry point are expanded inside `run-passes` (section 9).
   Each body is compiled to a quoted ilasm datum by `tui-method` and `tui-main`.
5. **Program.** The module defines `prog` with `make-tui-program`, providing the references, the
   methods, and the entry point. It `provide`s it as `tui-program` and adds
   `(module+ main (run-tui-program prog))`.
6. **Run.** `run-tui-program` assembles the program with Rangi and runs it. `racket m.rkt` runs the
   `main` submodule. `tui-write-exe` writes the `.exe` without running it.

### Line markers and the PDB

Each statement of a function body and of the entry point is preceded by a Rangi `.line` directive
that gives its source span. Rangi writes these into the PDB beside the exe, so a debugger can map
IL offsets to lines of the `.tui` file.

- The span comes from the statement's `syntax-position` and `syntax-span`. Racket counts a `\r\n`
  pair as one position, so `record-line-source!` normalizes line endings before it computes the
  line starts. Columns are 0-based character counts. Lines are 1-based.
- Only statements written in the file get a marker. `syntax-original?` is false for everything a
  Tui form expands to, so the test is whether the position lies inside a compiled module-level form
  from the same file. A statement that a macro template produces has template positions and no
  marker.
- Named locals and parameters are written too. A parameter's name is a `(param () TYPE NAME)` entry
  in the method signature, and a local's name is a `(TYPE NAME)` entry in `.locals`. Lets, catch
  variables and captured-argument cells use the user's name. Compiler temporaries stay unnamed.
  Rangi requires unique local names per method, so a shadowed name gets a suffix (`acc_1`).
- `tui-program->il` declares the image `.format pe32+`, so it is an AMD64 exe. The .NET Framework
  debugger only attaches to x64 processes, and the exes therefore do not run on 32-bit Windows. It
  also adds an assembly-level `DebuggableAttribute` with the JIT tracking and optimizations
  disabled, so locals stay alive and Just My Code can step into the code.
- The REPL emits no markers, since it has no module.
- The `.line` file name is the source's absolute path, so the PDB, and so the exe's bytes, depend on
  where the repository is.
- `run-tui-main` deletes the PDB with the temporary exe.

`build.rkt` writes the exe and its PDB, and the VS Code debug provider uses it (section 4, the VS
Code companion).

## 4. Typed fragments and the `tc` contract

Every Tui expression expands to `(tui-il (type instructions))`, a datum wrapped in syntax:

- `type` is a Tui type datum (section 5).
- `instructions` is a list of ilasm instructions for the expression's value.

Two functions in `main.rkt` produce and read these:

- `il ty instrs` builds the `tui-il` form and stamps it with `(syntax-property ... 'tui-type ty)`.
  This is the only place a typed fragment is built.
- `tc stx [ctx]` calls `local-expand` with `tui-il` as the stopping point and returns
  `(values type instructions)`. It reads the type from the `'tui-type` property, so a type can be
  read without unpacking the payload.

`tc/expect stx to` expands `stx`, checks it against `to` with `coercion`, and appends any
conversion instructions. It raises `type mismatch: expected ..., got ...` at `stx` on failure. It
passes the expected type to `stx` as the `'tui-expected` property. This is how a `fn` with no
delegate name learns its type (section 10).

### Types as syntax properties

The `'tui-type` property is the static type of every expanded expression. It survives
`local-expand`, so a macro can read it. The phase-1 API exposes this as `tui-type-of`, which
expands an expression and returns its type. Two rules keep the types consistent:

- Only types known at compile time are used. There is no unification and no inference from
  argument types beyond the rule in section 6.
- A function's type comes from its declaration (`fun-header`, `fun-declaration`), or from its body when the return type is omitted (section 6).

A macro that builds `(tui-il ...)` by hand bypasses `il`, so it has no `'tui-type` property. In
that case `tc` raises `internal: untyped fragment`. Build fragments with `il` or through ordinary
Tui forms.

### Hover tooltips

Each `tc` call also records its expression's type for hover. `record-tooltip!` keys the type by
source file and by the expression's `(position . span)`, and keeps the first entry for a key.
Expressions with no source or no span are skipped. The table is per file, so a required module
does not disturb the module being expanded.

Binding positions and call arguments are recorded too: `compile-body` records each bound name
(function parameters and locals), `tui-let` records each `let` name, `tui-set!` records the target of
a `set!`, and `tc-args` records each argument leaf. `tc` and `tc-args` take the original syntax as an origin, since
`local-expand` before `tc` produces syntax with no source that would be dropped.

`tooltip-leaves` turns each entry into leaves `(vector stx start end text)`, following Typed
Racket's placement. A compound expression is marked on its parens only, so an inner expression's
leaf does not overlap its outer one. An atom is marked over its whole text. Positions are 0-based
character offsets, which is what racket-langserver expects.

racket-langserver shows the leaf text as Markdown with no code fence, so the `<` and `>` of a
generic type are escaped with a backslash (`System.Collections.Generic.List\<int32\>`). Unescaped,
`<int32>` is read as an HTML tag and disappears from the hover. Check Syntax in DrRacket shows the
backslashes as they are.

`tui-module-begin` clears the file's table, and `tui-tooltips` runs after `prog` in the module's
expansion. It logs the leaves on `online-check-syntax` as a carrier syntax object, with the
`'mouse-over-tooltips` property. racket-langserver reads that log as hover text, and the carrier
stays in the expansion so Check Syntax can read the same property. The log message is prefixed
with the topic, and its suffix routes it to the hover path, not Typed Racket's.

### Scope table and the metadata helper

Completion needs the binders visible at a position. Parameters (`compile-body`, `compile-function`),
`let` names (`tui-let`) and module-level `define`s call `record-scope!`. Each record is
`(vector name type-text position span start end)`, where `start` and `end` bound the part of the
file the binder is visible in (`end` is exclusive, and module-level binders run to `+inf.0`). The
first record for a binder position wins, as with hover.

Macro-time tables live in the phase-1 instance of `main.rkt`, which a separate process cannot read.
So `%tui-scopes` logs `(list src records references opens)` on the `tui-scopes` topic of the
`info` level after each expansion, and the references and opens are included so the helper can
configure the metadata it answers with.

[meta-server.rkt](/c:/repos/tui/private/meta-server.rkt) is a long-lived process that speaks JSON
lines on stdio. It expands each `scopes` request's text (`#lang` is skipped, and the forms keep
their positions) in one namespace, and keeps the scopes of the last good expansion for each file.
Relative requires such as `(require "fnlib.rkt")` resolve against the file's directory, not the
helper's working directory, so the file's own path must be sent.
A failed expansion replies `ok: false` with `kept` and leaves the snapshot alone. Requests: 

- `names` (`offset`): the visible values and functions at a 0-based offset, innermost first.
- `types`: the public types of the references, with the namespace and whether it is opened.
- `members` (`offset`, `receiver`): the members of a visible value, the instance members of a numeric literal such as `1` or `1.5`, or the static members of a type name.
  Both include the members inherited from base classes, and Object's members for interfaces. A method
  hides an inherited one with the same name and parameter types.
  Getters with no parameters are properties. Other methods are listed with their overloads.

The offset is a character offset. VS Code counts UTF-16 units, so the two agree only for text
outside the astral planes.

#### The VS Code companion

[editors/vscode-tui](/c:/repos/tui/editors/vscode-tui) is a VS Code extension that runs the helper
(`racket private/meta-server.rkt`, found from the `tui.metaServerPath` setting or the workspace).
It is plain JavaScript with no build step, so it runs in VS Code's own Node without an install.
The plan called for TypeScript; this was chosen because the repository has no Node toolchain.

- `lib/syntax.js` decides whether a document is a Tui module (its first line is `#lang tui`) and
  finds a dotted receiver before the cursor, such as `names.Co`. These are pure functions.
- `lib/metaClient.js` runs one helper process and matches replies to requests by `id`. Requests time
  out after 20 seconds. A helper that exits fails the requests it owed.
- `lib/service.js` sends `scopes` only when a document's version changes, so names, types and
  members share one expansion. Types are cached per version.
- `lib/completion.js` answers the completion provider. In a member position it offers the receiver's
  members, with the typed prefix as the range. Each method overload is its own item, with its
  parameter types in the label, so they are visible without focusing each one. Elsewhere it offers
  the visible values and the opened arity-0 types, ranked ahead of racket-langserver's name-only list.
- `lib/browse.js` is the `Tui: Browse Members` command. It asks for a value or type, then a member,
  and inserts the member in the dotted form the compiler reads.
- `lib/debug.js` holds the pure parts of debugging: the exe path, the `build.rkt` arguments, and the
  launch configuration. The `tui` debug type builds the source with `build.rkt` into the OS temp
  directory, then returns a `clr` launch for the exe, which the C# extension runs with its PDB.

`extension.js` wires these up. The tests in `editors/vscode-tui/test` run under VS Code's
Electron in Node mode, with a stand-in `vscode` module and the real helper. The extension host
itself is not exercised.

## 5. Type representation

A Tui type is a datum:

- A primitive symbol: `void`, `bool`, `char`, `int32`, `int64`, `float64`, `string`, `object`.
- `(class asm ns name args)` for a CLR class, with `args` the closed type arguments.
- `(szarray t)` for a single-dimension array, and `(mvar n)` for the nth type parameter of a
  generic method, which exists only before instantiation. An array is written `|int32[]|` in source
  and `|int32[][]|` for a jagged array. `string->tui-type` and `ann->type` read both forms.

The functions that move between syntax and datums:

- `ann->type stx` reads a type annotation, such as `int` or `|Func<int32, int32>|`.
- `string->tui-type s` reads a type named in source text. This includes primitive aliases, dotted
  names, and generic instances such as `System.Collections.Generic.List<int32>`.
- `resolve-type-name id` (phase 1, exported as `tui-resolve-type`) turns an identifier into a type.
- `tui-type->il t` gives the IL type form, with TypeSpec tokens for constructed types (section 11).
- `owner->il t` gives the IL form for a class that holds a member.
- `tui-type->string t` gives the source spelling used in error messages.

Names with commas must use pipes, for example `|Func<int32, int32>|`, because the reader splits a
symbol at its comma.

## 6. Type inference

Inference uses only what the compiler already knows: the types of expanded expressions, the expected
type of a position, and CLR metadata. Nothing is unified. A type argument is fixed only by a direct
match against an argument. If it cannot be fixed that way, the call is rejected. The compiler does
not guess.

### Sources of types

| Source | Carried by | Used for |
| --- | --- | --- |
| Expanded expression | `'tui-type` on `tui-il` (`il`, `tc`) | The type of every expression, flowing upward (section 4) |
| Expected type | `'tui-expected` (`tc/expect`) | A `fn` with no delegate name (section 10), and a bare `null` (section 4) |
| Declaration | annotations, `fun-header`, module `define` | Parameters, return types, module values |
| CLR metadata | `clr-method` from `find-static-methods` and `find-instance-methods` | Overload candidates, parameter types, delegate `Invoke` arity |
| Declaration | annotations, `fun-header`, module `define` | Parameters, return types, module values |
| Body | the last expression of an unannotated function or module value, inferred on first use | Return types and module values with no annotation |
| Bare fn placeholder | type `(lambda n)` and struct `deferred-fn` | A bare `fn` argument before its overload is chosen |
| Bare null placeholder | type `'null-arg` (`null-placeholder`) and struct `deferred-fn` | A bare `null` argument before its overload is chosen |

### Inferred return types and module values

A `define` with no return type, and a module `define` with no annotation, takes its type from its
expression. `make-tui-fun` and `make-tui-global` keep the definition in the binding (`def`), and the
type is found when it is first needed (`tui-fun-result-type`, `global-result-type`, both through
`infer-once!`). The type comes from `infer-function-type` or `infer-value-type`, which type the body
or initializer in a discovery pass with no IL and no lambda lifting. The result is cached in the
binding, and the code pass uses that same type, so callers and the callee agree. A function is typed
with no value scope, because any function can be called from any module value's initializer.

A cache entry is `'pending` while its body is typed. A reference to a definition that is still
pending is a cycle, and it is an error (`cannot infer the return type of f, which depends on
itself`). So a recursive function, direct or mutual, needs a return type annotation. A definition
can refer to any function or module value in the module, because every binding is set before any
body is typed. A tail `null` or bare `fn` has no expected type in an unannotated body, so write
`(null-of T)` or annotate the return type. An `if` with a bare `null` in one branch is the exception:
`tui-if` takes the type from the other branch, or from the expected type when there is one.

### Bare fn placeholder

A call to a .NET method (`compile-static-call`, `compile-instance-call`, `tui-new`) runs in three steps.

1. **Type the arguments** (`tc-args`). Each argument is expanded and typed. A bare `fn` is detected
   after `local-expand` by its `tui-fn` form. It gets the type `(lambda n)`, where n is its arity,
   and is kept as a `deferred-fn`.
2. **Choose the overload** (`resolve-overload`). Each candidate is closed by `infer-targs`, and each
   argument is scored with `arg-coercion`. A placeholder matches any delegate whose `Invoke` has n
   parameters, at cost 0. A bare null matches any reference parameter (a class, `string` or `object`)
   at cost 0, and no value type. The lowest total cost wins. When costs tie, a non-generic candidate beats a
   generic one, as in C#. Groups are tried in order: a derived type's methods come before its bases',
   and the first group with an applicable candidate wins. A tie that remains is an error, so
   `Console.WriteLine null` is ambiguous.
3. **Finish the call** (`finish-call`). If every type argument is fixed, each argument is converted to
   its parameter type (`finish-args`), and each deferred `fn` or `null` is compiled against that type
   with `tc/expect`. If a type argument is still open, the bare `fn`s fix it first (`compile-partial-fn`),
   then the arguments are converted. A null fixes no type argument, so it is compiled after the method
   is closed. An argument whose type is still open is an error.

### Member access syntax

Four forms reach the same CLR call code:

- `(.Method recv args ...)`: an instance call. `recv` is any expression.
- `(Type.Method args ...)`: a static call. `Type` is a type name, possibly dotted, such as `System.Console`.
- `(a.Method args ...)`: an instance call on `a`, when `a` is a local or parameter. The receiver is
  `a`. A local shadows a type of the same name, so with a local `Console`, `Console.Length` reads the
  local. `a` must be a single segment. A numeric literal can also be `a`: `1.ToString` and
  `1.5.ToString` are instance calls on an `int32`, `int64` or `float64` literal (`5000000000` is
  `int64`). The reader keeps `1.ToString` as one symbol, so the compiler splits it at the last dot
  and types the literal from its text. Strings, chars and booleans have no dotted form. Use the
  prefix form `(.Length "x")` or a threaded step `(~> "x" (.Length))`.
- bare `a.Name` and `Type.Name`, with no parentheses: a member read with no arguments. These expand
  to a zero-argument call through `tui-top`, which hands them to `tui-app`.

A member read, with or without arguments, uses a method `Name` when one exists, and the usual overload
rules then apply. Otherwise, when there are no arguments, it falls back to the property getter
`get_Name`. So `s.Length` reads the `Length` property of a string, and `xs.Count` reads `get_Count`.

Threading works with the same forms. A bare clause `.Name` in `~>` or `~>>` becomes `(.Name prev)`,
so `(~> "tui" .Length)` is a property read on the threaded value. A primitive value in the chain is
boxed and its members are looked up on `object`. A struct or enum value is unboxed to a managed
pointer for its own instance members. An inherited member such as `ToString` from `Enum` is called on
the boxed object, since a managed pointer is not an object reference.

Hover: `a.Name` is marked as one expression, and its receiver `a` has its own leaf. A threaded step is
a compound expression, so it is marked on its first and last characters only, as for any form in the
hover model. The middle of a threaded step's name has no tooltip.

### Static fields and enum members

`Type.Name` with no arguments reads a public static field when the type has one of that name, and
only then falls back to a property or method. A literal field (a `const`) is emitted as its value
with `ldc`, `ldstr` or `ldnull`. An enum member is a literal field typed as the enum, so it is emitted
as the enum's underlying integer. Other static fields are read with `ldsfld`. Tui cannot define an
enum; it only passes enum values around, compares them with `=`, and reads their members by name.
Constants of a type Tui cannot emit, such as `Byte`, are not read. They are missing from completion
and give an unknown member error.

### Arrays and loops

- `(array T e ...)` builds an array of T from its elements. `(new-array T n)` builds one of length n
  with default elements. `(aref a i)` reads an element, and `(set! (aref a i) v)` writes one. Index
  and length are `int32`. `a.Length` reads the length, since an array's members are System.Array's.
- `(for-each [x xs] body ...)` loops over an array, an `IEnumerable<T>` (such as a `List<T>`), or a
  string. The element type comes from the array, the enumerable's element type, or `char` for a
  string. The loop variable can be annotated, as in `[x : int32 xs]`, and the annotation must match
  the element type. The loop lowers to `GetEnumerator`, `MoveNext` and `Current` on an enumerator,
  or to indexed access for an array. `MoveNext` is called on the non-generic
  `System.Collections.IEnumerator`, since `IEnumerator<T>` has no base class and a lookup through its
  bases does not find the method.
- An array converts to `IEnumerable<T>` for its element type, so `Enumerable.Contains` and other
  LINQ methods take it. Section 7 gives the cost.

### Fixing type arguments (`infer-targs`)

Each parameter is matched against its argument, in order:

- A parameter that is exactly `(mvar n)` fixes `!!n` to the argument's type. Every use of `!!n` must
  agree (`bind-type`).
- A generic instance parameter, such as `IEnumerable<!!0>`, matches the argument's supertype with the
  same generic definition (`matching-supertype`). A `List<int32>` argument fixes `!!0` to `int32`.
- A placeholder binds nothing. A type argument that occurs only in bare-fn parameters stays open
  (`lambda-only?`). The fn's body fixes it in `finish-call`.
- Explicit type arguments (`Name<A>`) are used as given and are not bound from the arguments, so an
  argument that differs is left to the coercion check. The list may be shorter than the arity, and a
  `_` (or a missing entry) is inferred as below: `|Select<int32, _>|` and `Select<_>` both work. A
  list longer than the arity matches no overload.

Other parameters are checked by `coercion` after the method is closed. Nothing else is inferred.

### Parameters from a call in the body (Phase D)

An unannotated `fn` parameter can take its type from a call in the body (`body-param-types`, run by
`compile-lambda` when no delegate is given). A parameter qualifies when it is a direct argument of a
call, and that name and arity have exactly one overload. The overload must be non-generic, and its
parameter type at that position must be closed. Calls are `Type.Method` (static) and `.Method recv`
(instance, where `recv` is a variable with a known type). The scan does not enter nested `fn`, `let`,
`let*`, `letrec` or `define`. Overloaded names, such as `Console.WriteLine`, give nothing, so the
parameter must be annotated.

### Internal representation

- `(mvar n)` is an unfixed method type parameter. `(lambda n)` is a bare-fn placeholder. A `#f` slot
  in `infer-targs` means open.
- `has-mvar?` tests for open parameters. `erase-mvars` replaces them with `object` before a type's
  shape is inspected (`delegate-param-args`, `delegate-arity`). `tui-type->il` must never see an
  `mvar`.
- `subst-mvars` and `instantiate-method` close a method. A method with an open type argument is never
  emitted.
- Functions that do the work: `clr.rkt` has `resolve-overload`, `infer-targs`, `bind-type`,
  `matching-supertype`, `lambda-only?`, `instantiate-method`, `arg-coercion`. `main.rkt` has `tc-args`,
  `finish-args`, `compile-partial-fn`, `finish-call`, `body-param-types`.

### Examples

- `(Enumerable.Where<int32> xs (fn (x) (> x 2)))` is accepted. The explicit type argument fixes it.
- `(Enumerable.Where xs even?)` with `even?` a variable of type `Func<int32, bool>` is accepted. The type arguments are bound from the argument types. A generic parameter matched against a class with a base chain (for example `List<T>` or a delegate, whose chain ends at `object`) skips the non-class `object` entry in `matching-supertype`.
- `(Enumerable.Where xs (fn (x) (> x 2)))` is accepted when `xs` is a `List<int32>`. Its
  `IEnumerable<int32>` base fixes `!!0`, and the `fn` takes `Func<int32, bool>`.
- `(Enumerable.Sum (Enumerable.Select xs (fn (x) (* x 2))))` fixes `Select`'s `TSource` from `xs` and
  its `TResult` from the body.
- `(let ([f (fn (s) (String.IsNullOrEmpty s))]) ...)` infers `s : string`.
- `String.Concat` and `Enumerable.Max` choose their non-generic overloads on ties.

Errors from this section are listed in section 14.

## 7. Overloads and conversions

`coercion from to` returns `(cons cost instructions)` or `#f`. `tc/expect` uses it for arguments
and returns:

| Conversion | Cost | Instructions |
| --- | --- | --- |
| identical types | 0 | none |
| `int32` to `int64` | 1 | `conv.i8` |
| `int32` or `int64` to `float64` | 1 | `conv.r8` |
| class or `string` to a base class or interface | 1 | none |
| reference type to `object` | 2 | none |
| primitive value type to `object` | 2 | `box` |
| struct or enum to `object` | 2 | `box` |
| struct or enum to an interface or base class it implements | 1 | `box` |
| array to `IEnumerable<T>` for its element type | 1 | none |

Reference upcasts emit no IL, because the verifier accepts a reference to a base type. The cost of
`object` is higher than any base class or interface, so `List<T>` prefers `IEnumerable<T>` over
`object`. An array is a reference type whose members are System.Array's, and it has no upcast to
another array type.

`resolve-overload groups arg-types describe explicit` scores each candidate. A candidate must have
the right arity and every argument must coerce. Its score is the sum of the costs. Groups are tried
in order, and the first group with any applicable candidate wins. The lowest score wins. Among equal
scores a non-generic candidate wins over a generic one. A tie that remains is an error,
`ambiguous call to ...`. No candidate gives `no overload of ... accepts (...)`. Section 6 describes
how type arguments are fixed before scoring.

## 8. Module-level values

A module value is a static field on the module class. Syntax:

```racket
(define name : type expr)
(define name expr)                ; type inferred from expr
```

The type annotation is optional. Without one, the type is inferred from `expr` as for a function
return type (section 6). A value that infers to `void` is an error. Each value gets:

- a static field `name`, declared with `.field (public static)`;
- a static method `__init_name` that computes `expr`.

The entry point runs the initializers in source order before the body, using
`value-inits`. Each initializer assigns its field with `stsfld`.

Rules:

- A function may read any module value.
- An initializer may read only the values defined before it. `value-scope` holds the names defined
  so far, and `check-global-scope` raises `module value used before it is defined` on a direct read
  of a later value. Reads through a function call are not checked, so `(define a : int (f))` where
  `f` reads a later value is not caught. An inferred value's type is found in the same discovery
  pass, with the same scope, so an inferred value cannot read a later value directly either.
- A lambda that reads a module value needs no capture, because the value is a static field, and it
  sees the current value.
- `set!` on a module value writes the field (`tui-set!` handles `tui-global`).
- A bare `null` initializer has no type to infer, so `infer-value-type` raises
  `module value null needs a type annotation`. An annotated `(define name : string null)` is fine.
- A module value can be defined only in a module. A REPL interaction that defines one raises
  `module values can only be defined in a module`.

The module-level machinery is `make-tui-global` (a `tui-global` struct bound with `define-syntax`),
`value-parts` (builds the field and initializer), the `tui-value-decls` macro (returns the field and
initializer pairs for the method list), and `compile-main`, which takes the initializers. The field
access form is `(ldsfld Program name type)`. Section 11 explains `Program`.

### Records

A record is a sealed class with the same module-level rules. Syntax:

```racket
(record Name [field : type] ...)
```

`record-declaration` in `main.rkt` parses the form and calls `declare-record!` in `private/clr.rkt`,
which declares the type in the Rangi type environment at expansion time (phase 0). The descriptor has a
public `.ctor` that takes every field in order, a private field per name, and public `get_name` and
`set_name` methods. The type checker then sees the record like any other class. `record-class-datum`
builds the IL class (`ldfld`, `stfld`, and the accessors) and `tui-module-begin` adds it to the program
with `tui-program-add-class!`.

Rules and details:

- `tui-module-begin` calls `reset-local-declarations!` before it declares the records, so the
  declarations of one module do not leak into the next.
- A record's name is referenced bare (`Name`) in type positions, and `local-type-ref` finds it before
  Rangi's namespace search, which does not look in the empty namespace. Fields are read as `p.name` and
  `(p.get_name)`, and written as `(set! p.name e)`, which `tui-set!` rewrites to `(p.set_name e)`.
- A record in a function body or an expression raises `record is only allowed at module level`, and
  so does one in a REPL interaction.
- A field cannot be `void` or repeat a name. A record name can be declared once per module.
- A field can have an earlier record as its type. A forward reference fails as `unknown type`.
- A record name works in type-pattern positions too. `define-type-syntax-class` can name it (`Point`),
  or match it structurally with `has (.get_x () : int32)`, since the pattern member names are the
  CLR metadata names. `examples/records.rkt` has these, and `type-matches?` and `type-match` run on them.
- Owner references to a type in the same assembly use `(Name)` (for `newobj` and `call`). A type
  position uses the bare symbol, `(class Name)`, which is what Rangi's ilasm `check-type` accepts.
  Only `tui-type->il` differs between the two.


## 9. Two-pass expansion and lambda lifting

A method that contains a lambda is expanded twice. The first pass records the variables that
lambdas read. The second pass expands the method again with those variables as cells. `run-passes`
drives this. `current-pass` holds the pass's data, including `pass-cells`.

Consequences:

- Expansion must be deterministic. A macro that reads mutable phase-1 state can break the second
  pass.
- A name a macro introduces into a lambda is captured like any other name.
- Hover tooltips are recorded in both passes. The first recording of a span wins, so the two
  passes must agree on types, which they do because expansion is deterministic.
- `%tui-tooltips` is a `define` after `prog`. A bare macro use at module level expands before
  `prog`'s right-hand side, so the table would be empty.

Lambda classes are collected in the `lifted-classes` box during expansion. `tui-lifting` and
`tui-lifts-into` place them into the module, and `tui-module-begin` resets the box. The full design
is in [captures.md](captures.md) sections 4 and 5.

## 10. First-class lambdas

`(fn DELEGATE (param ...) body ...)` builds a delegate. The delegate may be omitted when the
expected type is a delegate, for example the tail of a `define` whose return type is a delegate, or
an argument of a method that takes one. `compile-lambda` produces:

- a generated class with one field per captured cell and an `Invoke` method;
- a delegate over `Invoke` on an instance of that class.

Without a delegate name, `fn` takes its type in one of four ways:

- **From the expected type** (`tc/expect` sets `'tui-expected`): a `define` return type or a
  parameter of a Tui function.
- **From the chosen overload** (Phase B): a bare `fn` argument of a CLR call, constructor, or
  instance call. `tc-args` detects it with `local-expand` (stop ids `tui-il` and `tui-fn`) and
  types it as a `lambda-placeholder` of its arity. `resolve-overload` matches a placeholder against
  any delegate parameter whose `Invoke` has that arity (`arg-coercion`). After the overload is
  chosen, `finish-args` compiles the `fn` against that parameter type with `tc/expect`. If two
  overloads match, the call is ambiguous and is rejected.
- **From the body** (Phase A): each parameter is annotated (`(fn ([x : int]) body)`), or left
  unannotated for Phase D to fill in. Annotations and Phase D results are combined, and any
  parameter still without a type is an error. The body's type picks `Action<...>` when it is
  `void`, otherwise `Func<..., R>`. The delegate is built with the `Func` or `Action` of those
  types from `compile-lambda`, so no `|Func<...>|` name is needed.
- **Parameters from a call in the body** (Phase D, `body-param-types`): an unannotated parameter
  that is a direct argument of a call in the body takes that call's parameter type, when the name
  and arity have exactly one closed, non-generic overload. Calls are `Type.Method` and
  `.Method recv` with a variable receiver, and `(f x)` for a delegate local or module value, whose
  parameters come from its `Invoke` method. The scan skips nested binders, so it does not look
  inside `fn`, `let`, `let*`, `letrec` or `define`. Example: `(fn (s) (String.IsNullOrEmpty s))`.
  Overloaded methods such as `Console.WriteLine` give no inference, so the parameter must be
  annotated.

- **From a generic call's body** (Phase E): a bare `fn` argument can fix a method type argument that
  no other argument mentions, such as `TResult` in `Enumerable.Select`. The `fn`'s parameters take
  the delegate's already-known types, and its body type fixes the rest. Section 6 describes the rule
  and its limits.

A lambda is a value. It can be stored with `let`, passed as an argument, returned, and called. A
delegate value `f`, whether a local, an argument or a module value, is called as `(f x)` or as
`(.Invoke f x)`, and both compile to the same call. Calling a name that is not a delegate is an error.
Functions are called directly, which emits `call` on `Program`.

Captures are shared, not copied. Each run of a `let` makes new variables. See
[captures.md](captures.md) for cells, env fields, and invariants.

## 11. Rangi and IL

Tui generates ilasm datums and Rangi assembles them. The forms this repository depends on:

| Use | Form |
| --- | --- |
| module field read | `(ldsfld Program name type)` |
| module field write | `(stsfld Program name type)` |
| module field declaration | `(.field (public static) type name)` |
| static call | `(call ret (Program) name (params))` |
| constructed type operand | TypeSpec token (Rangi commit `f84aa29`, "Emit TypeSpec tokens for constructed operands") |
| source line | `(.line l1 c1 l2 c2 "file")`, 1-based lines, columns 0..#xFFFF (Rangi writes the PDB) |

`Program` is how Rangi names the `<Module>` class. Rangi moves the module's static methods into
`Program`, so Tui code refers to `Program`, not `<Module>`.

Relevant Rangi source: `C:\repos\rangi\src\cli\lower.rkt` (field and call lowering) and
`C:\repos\rangi\src\ilasm\syntax.rkt`. If Rangi changes these forms, update `tui-program->il`
and `compile-main` in this repository.

## 12. Phase-1 API

Phase 1 is the compile-time side. Helpers exported `for-syntax` from `main.rkt` let user macros
read types and metadata:

| Helper | Purpose |
| --- | --- |
| `tui-type-of stx` | the Tui type of an expression (type checks it, does not run it) |
| `tui-resolve-type id` | the type named by an identifier |
| `tui-member-return-type t name` | return type of the first zero-argument instance method `name` on `t`, from closed-type metadata |
| `tui-has-default-ctor? t` | whether `t` has a public parameterless constructor |
| `tui-static-overloads type-name method-name` | number of public static overloads |
| `tui-instance-overloads t name` | number of public instance overloads on `t` and its bases |
| `tui-member-names t` | sorted distinct public instance method names |
| `tui-type->string t` | source spelling of a type |
| `class-of t` | the CLR class descriptor for `t` |
| `supertypes t` | transitive base classes and interfaces, as Tui types |

Syntax classes for macro arguments: `typedExpr` (`x.Type`) and `typeName` (`x.Type`). Richer
constraints are written with `define-type-syntax-class` (section 12.1).

Limits: the helpers see only members whose signatures use types Tui models, so overload counts are
smaller than the full .NET counts.

### 12.1 Type patterns

`define-type-syntax-class` names a type constraint and makes it a syntax class. `type-match` and
`type-matches?` test the same patterns at phase 1. `private/type-patterns.rkt` holds the solver
(phase 0, so its helpers run at phase 1 as well). `private/type-syntax-class.rkt` compiles patterns
and defines the three macros. `main.rkt` only re-exports them.

Pattern grammar:

```
pat     ::= _                      any type
          | ?id                    binder; every use of the same ?id must be the same type
          | prim                   void bool char int32 int64 float64 string object (and aliases)
          | Name                   a closed type, named as in an annotation
          | UserName               a pattern defined with define-type-syntax-class
          | (Gen pat ...)          a closed generic instance; one pattern per type argument
          | (open Gen)             any closed instance of Gen, whatever its arguments
          | (szarray pat)          a single-dimension array
          | (<: pat)               a base class or interface, at any depth, that matches pat
          | (and pat ...)          all match; bindings flow left to right
          | (or pat ...)           the first branch that matches
          | (not pat)              does not match; binds nothing
          | (opt pat)              pat if it matches, otherwise its ?ids are #f
          | (exactly expr)         the type equal to the phase-1 value of expr
          | (? pred)               pred, a procedure on types, returns true
          | (? pred pat)           pred holds and pat matches
          | (app fn pat)           pat matches (fn type)
          | (has member ...)       each member matches a public overload
member  ::= (.Name (param ...) : ret)            instance method
          | (.Name (?T ...) (param ...) : ret)   instance method with method type parameters
          | (static .Name ...)                   static form of either
          | (.ctor (param ...))                  public constructor
```

Names are CLR metadata names (`.get_Item`, not `.Item`). A `?T` in a member refers to the outer
binder unless it is declared in the member's `(?T ...)` list. A declared `?T` is local to the
member and matches the method's own type parameter; it is not exposed as an attribute.

Syntax classes and macros:

- `(define-type-syntax-class Name [#:description "text"] pat)` defines `Name`. A pattern variable
  `x:Name` accepts any expression and has `x.Type`, the type the checker gives it, plus one
  attribute per binder written directly in `pat`. The description appears in error messages; without
  one, the printed pattern is used.
- A named pattern used inside another contributes its test, but its binders are not exposed on the
  outer class.
- `(type-match texpr [pat body ...] ... [else body ...])` runs at phase 1 and binds each `?id` in
  its clause's body. Without a matching clause and without `else`, it raises an error.
- `(type-matches? texpr pat)` returns a phase-1 boolean.

Generic arguments match exactly, so generics are invariant. A `List<int32>` argument does not match
`IEnumerable<int32>`. Write `<:` in the argument position to accept a subtype:
`(<: (IEnumerable (<: (IEnumerable ?X))))` matches `List<List<int32>>` and binds `?X` to `int32`.

Excerpts from `examples/lists.rkt` and `examples/type-patterns.rkt`. The full files show the rest:

```racket
; a list: IEnumerable<T> with get_Count and get_Item(int32) : T (lists.rkt)
(define-type-syntax-class typedList
  (and (<: (System.Collections.Generic.IEnumerable ?Elem))
       (has (.get_Count () : int32))
       (has (.get_Item (int32) : ?Elem))))

; open and closed generics, with a named pattern inside another one
(define-type-syntax-class dictLike
  (<: (System.Collections.Generic.IDictionary ?K ?V)))
(define-type-syntax-class countedDict
  (and dictLike (has (.get_Count () : int32))))

; one binder shared by an interface and a member parameter
(define-type-syntax-class growableSeq
  (and (<: (System.Collections.Generic.IEnumerable ?T))
       (has (.Add (?T) : void))))

; a method type parameter, solved against the signature but not exposed
(define-type-syntax-class convertible
  (has (.ConvertAll (?R) ((System.Converter ?T ?R)) : (System.Collections.Generic.List ?R))))

; static members
(define-type-syntax-class stringLike
  (has (static .IsNullOrEmpty (string) : bool)))

; combinators, a predicate, and a phase-1 value
(define-type-syntax-class scalar (or string int32 int64 bool))
(define-type-syntax-class numeric
  (? (lambda (t) (and (memq t '(int32 int64 float64)) #t))))
(define-for-syntax the-int-type 'int32)
(define-type-syntax-class exactlyInt32 (exactly the-int-type))

; type-match with nested open, binders, named patterns, nested generics and an else clause
(define-syntax-parser describe
  [(_ x:typedExpr)
   (define msg
     (type-match (attribute x.Type)
       [(System.Collections.Generic.Dictionary (open System.Collections.Generic.List) ?V)
        (format "a dictionary keyed by a list, with ~a values" (tui-type->string V))]
       [(System.Collections.Generic.List (System.Collections.Generic.List ?X))
        (format "a list of lists of ~a" (tui-type->string X))]
       [(<: (System.Collections.Generic.IEnumerable (or string int32 int64 bool)))
        "a sequence of scalars"]
       [scalar "a scalar"]
       [else "something else"]))
   #`#,msg])
```

Limits:

- Only members whose signatures use types Tui models are visible, so overload sets are smaller than
  the full .NET ones.
- `(open Gen)` checks the name and that the type has type arguments. It does not check the arity.
- `(or ...)` takes the first branch that matches. Its branches are not checked to bind the same
  `?id`s.
- `szarray` patterns compile, and arrays are now built by `array` and `new-array`, but no type-pattern
  example uses `szarray`.
- A named pattern exported from one module and used from another has not been tested.

## 13. REPL

`tui-top-interaction` expands each REPL interaction. It classifies the form with `form-kind`:

- function `define`, `using`, `open` and macros are accepted;
- module value `define` is rejected (section 8);
- an expression is compiled with `tui-main/print` and run with `run-tui-main`.

A `define` adds a method to the running program, and `using` and `open` update the configuration.
Lambdas in an interaction are lifted into the program the same way as in a module, and a test
covers this (`the REPL lifts lambdas defined in an interaction`).

## 14. Errors

Errors are Racket syntax errors raised with `raise-syntax-error 'tui`, so they point at the source
form. Tests read them with `tui-error` (section 15).

Common messages (the exact wording is in `main.rkt` and `private/clr.rkt`):

- `type mismatch: expected T, got U`
- `no overload of M accepts (...)`
- `ambiguous call to M with (...)`
- `module value used before it is defined`
- `module values can only be defined in a module`
- `module values are not first-class in Tui; use the name`
- `expected (define name [: type] expr)`
- `expected (define (name [param : type] ...) [: type] body ...+)`
- `cannot infer the return type of f, which depends on itself` (a recursive definition with no annotation)
- `a module value cannot be void` (an unannotated module value whose expression is `void`)
- `module value null needs a type annotation, as in (define x : string null)` (a bare `null` module value with no annotation)
- `expected (fn [Delegate] (param ...) body ...+)`
- `fn without a delegate type needs an expected delegate type or annotated parameters`
- `fn has N parameters but X takes M`, `parameter n is T but D takes U`
- `parameter x is T but the delegate passes U` (a bare `fn` whose parameter annotation disagrees)
- `cannot infer the type of a parameter of this fn` (a bare `fn` whose delegate parameter is still open)
- `cannot infer the type arguments of M` (a generic CLR method whose type argument no argument fixes)
- `fn has type X, which does not fit Y` (a bare `fn` whose body type does not match the open parameter)
- `argument has type X, expected Y` (a bare `fn` argument that does not match the closed parameter)
- `function returns X but its body has type Y`
- `no overload of X accepts (fn/n, ...)` (a bare `fn/n` is a lambda of arity n)
- `not a variable`
- `unknown type`
- `internal: untyped fragment` (a hand-built `tui-il` form, section 4)
- `null needs an expected reference type` (a bare `null` with no expected type, such as in a `let`; use `null-of`)
- `null-of needs a reference type, got X` (`null-of` with a value type)
- `null cannot have type int32` (a bare `null` where a value type is expected)
- `null? needs a value, not a bare null`, `null? needs a reference type, got X`
- `cannot compare X with null`, `cannot compare null with null` (`=` with a null operand)
- `ambiguous call to X with (null)` (a bare `null` argument that more than one overload accepts)
- `try must be a statement; its value would be on the evaluation stack here` (a `try` inside an argument, operand, `set!` or `new`; see the exceptions note below)
- `try needs a body`, `try needs a catch or finally clause`, `expected a catch or finally clause`
- `finally must be the last clause of try`, `a catch-all clause must be the last catch clause`
- `catch needs an exception type, got X`, `cannot catch X` (a type defined in this assembly, or a generic type)
- `catch handler has type X, but the try has type Y`, `a finally body must be void, got X`
- `throw needs an exception, got X`, `rethrow is only allowed in a catch handler`
- `cannot bind a value that never completes`, `cannot assign a value that never completes` (a `never` expression, such as `throw`, used as a value)

Exceptions. `try` has three clauses: `(catch [e : T] body ...)` for one type, `(catch e body ...)`
for any exception (it must come last), and `(finally body ...)`, which must be void and come last.
The try's type is the body's type, and each handler must match it. A handler that ends in `throw`
or `rethrow` has type `never`, which fits any type and emits no store. `never` is only produced by
expressions and cannot be written as an annotation. `try` is only allowed where the IL evaluation
stack is empty: a function or lambda body, a module initializer, and the statement positions inside
them (`begin`, `let` body, `if` branches, `while`, `for-each`). The compiler marks those positions
with the `'tui-stmt` syntax property, so `try` in an argument is rejected. The handler's exception
variable is an ordinary local, or a cell if a lambda reads it. `rethrow` is only allowed directly in
a handler; a lambda inside a handler is a separate method and cannot rethrow the outer exception.

## 15. Tests

`tests/tui-test.rkt` contains the test helpers:

- `run-example file` runs the `main` submodule of an example and returns its output lines.
- `run-tui form ...` expands `(module m tui form ...)` in a fresh namespace and returns its output.
- `tui-error form ...` returns the syntax error message, or `#f` if there is none.

The REPL tests evaluate `(#%top-interaction . form)` in the module's namespace.

The hover tests expand a module read from a string (`expanded-tooltips`). They check the exact
(text, type) leaves, that binders (`let`, `set!`, `fn` parameters) and call arguments are marked,
and that the same carrier is logged on `online-check-syntax`. Generic types appear escaped
(`\<int32\>`) in the leaf text, which is what the tests expect.

Run the suite with `raco test tests\tui-test.rkt`. It takes about 4 minutes. Run `raco make
main.rkt` first if you have changed `main.rkt` or `private/*.rkt`.

`tests/meta-server-test.rkt` tests `handle-request` in process: the scope table (shadowing and
function records), member listing for a generic value and for a type, the public types of the
references, and the last-good snapshot after a failed expansion. It runs in a few seconds.

Stale compiled files cause struct mismatches. If you see one, delete the `compiled` directories
and run `raco make main.rkt`.

## 16. Examples

| Example | Shows | Test (in `tests/tui-test.rkt`) |
| --- | --- | --- |
| `basics.rkt` | recursion, loops, locals, calls into mscorlib | `examples run` |
| `captures.rkt` | shared captures, nesting, per-iteration `let` | `fn captures locals and arguments by reference`, `lambdas nest and share mutable captures ...` |
| `delegates.rkt` | `fn` for `Action`, `Func`, `Predicate` | `fn builds delegates for Action, Func and Predicate` |
| (inline tests) | annotated `fn` params take `Func` or `Action` from the body | `annotated fn parameters take Func or Action from the body type` |
| (inline tests) | bare `fn` argument takes its delegate type from the overload | `a bare fn argument takes its delegate type from the chosen overload` |
| (inline tests) | generic CLR method type arguments through base types | `generic methods bind type arguments through base types` |
| (inline tests) | bare `fn` argument fixes a method type parameter that only it mentions | `a bare fn argument fixes a method type parameter that only it mentions` |
| (inline tests) | unannotated `fn` parameter takes the type of the one overload its body fixes | `an unannotated fn parameter takes the type of the one overload a call in its body fixes` |
| (inline tests) | each source (expected type, overload, generic type argument, body, Phase D) with and without annotations, and mismatches | `each inference source works with and without annotations` |
| (inline tests) | generic parameters bound from a `List<T>` and from a delegate variable | `generic parameters are inferred from list and delegate variable arguments` |
| (inline tests) | type arguments as `_` or omitted at the end, and the errors for too many, uninferrable and conflicting ones | `type arguments may be _ or omitted at the end, and the rest are inferred` |
| `macros.rkt` | hygienic macros | `Tui macros are hygienic` |
| `types.rkt` | `default-of`, `type-name-of`, `null?` on a null argument, `typeof` | `default-of takes a type name ...`, `type-name-of expands ...`, `null is typed from the expected type and works as an argument`, `typeof gives the System.Type of a type name` |
| (inline tests) | `null-of` as a typed null in `let` and `set!`, and its rejections | `null-of declares a typed null where there is no expected type` |
| (inline tests) | unannotated module values of each type, an annotated null, and the null module value error | `module values infer their types, and a bare null needs an annotation` |
| (inline tests) | an `if` with a bare null branch in a module value, a function body, and a function with an expected type | `an if with a bare null branch takes its type from the other branch` |
| `lists.rkt` | `list-of`, `sum-of`, `typedList` built from the type-pattern language | `the list-of example runs`, `sum-of reads the element type ...`, `typedList no longer takes a dictionary's value type ...` |
| `type-patterns.rkt` | `define-type-syntax-class`, `type-match`, `type-matches?`: generics (open and closed), nested `<:`, named patterns, members and static members, method type parameters, combinators, predicates, `exactly` | `the type-patterns example exercises generic, member, named and combinator patterns` |
| `collect.rkt` | `type-match` choosing how to fill a collection at compile time: `List`, `HashSet`, `Dictionary`, and nested collections by recursing on the element type | `collect fills lists, sets, dictionaries and nested collections, and rejects non-collections` |
| `linq.rkt` | LINQ over stored lambdas, reference conversions | `LINQ works with stored lambdas, library macros and reference conversions` |
| `fnlib.rkt` | `compose` and `square` library macros | `compose checks that both arguments are Func<int32, int32>` |
| `phase1.rkt` | metadata helpers at expansion time | `phase 1 macros read overloads, supertypes and members from metadata` |
| `xml.rkt` | `System.Xml.Linq`: reading and building documents, a macro that walks an element path, an XML literal built at expansion time, a syntax class that names elements, and `type-match` on node kinds | `the XML example reads and builds documents with System.Xml.Linq` |
| `json.rkt` | a JSON parser and printer in one file: an index-linked node table, recursive descent with `while`, `json-literal` and `json-get` macros that build and walk JSON at expansion time, and `json-show` using `type-match` on `x.Type` | `the JSON example parses, walks and re-serialises JSON text` |
| `syntax-classes.rkt` | syntax classes, `define-simple-macro`, phase-1 helpers | `phase 1 definitions: syntax classes, simple macros and phase-1 helpers` |
| `threading.rkt` | `threading`'s `~>` and `~>>` | `threading's ~> and ~>> compile to ordinary Tui calls, and a bare .Name clause reads a member` |
| `values.rkt` | module values, `set!`, initialization order | `module values are static fields set in source order, and set! updates them` |
| `records.rkt` | record declarations: constructor, `get_`/`set_` properties, `set!` on a field, a record field of a record type, type-syntax-classes that name `Point` or match it structurally, and `type-match` on a record | `a record has a constructor, get_ and set_ properties, and can be passed and returned` |
| `arrays.rkt` | array literals, `new-array`, `aref` read and write, `for-each` over arrays, jagged arrays, `IEnumerable<T>`, strings and closures, `Length`, `Enumerable.Contains` | `arrays: literals, new-array, aref, for-each over arrays, IEnumerable and strings` |
| `enums.rkt` | enum parameters and members, `=` on enums, a module value of an enum type, `Int32.MaxValue`, a literal field, `Math.PI` | `enum members and static fields are read by qualified name` |
| `exceptions.rkt` | `try` with typed and catch-all handlers, `finally`, `throw`, `rethrow`, `try` in a `let` inside a function, a checked division that returns `never` on the error path | `the exceptions example runs`, plus the `try, throw and rethrow` tests |

## 17. Known limits

- Hover tooltips for two expressions with the same position and span show the first one recorded. A macro that copies a form without moving it can therefore hide the type of the copy.
- Hover text is escaped for racket-langserver's Markdown rendering, so DrRacket's Check Syntax shows `\<` and `\>` for generic types. This is accepted as is.
- Hover is checked through racket-langserver only, not in the DrRacket GUI.
- Hover and the scope table are not built for half-typed buffers. A failed expansion leaves the last good table in place, and the metadata helper answers from it. Completion at the cursor can therefore be a few edits out of date.
- The metadata helper's offsets are character offsets. Editors that count UTF-16 units (VS Code) disagree with them after an astral-plane character.
- Debugging in VS Code: breakpoints, stepping and return values have been checked in the Extension
  Development Host. Locals and parameters are now named in the PDB and need that check too.
  Compiler temporaries show without names, and a captured variable shows as the one-element array
  that holds it.
- The VS Code companion cannot browse generic types, since their type arguments are not known. They are reached through a value of the type. Member completion inserts names only, not argument lists.
- The metadata helper keeps a snapshot for every file it has been sent and has no close request, so it holds on to closed files until it exits.
- Inference is limited to exact `(mvar n)` parameters, base-type matches of generic instance
  parameters, and the bare `fn` body rule. There is no unification.
- A return type or module value type is inferred from its body only when no annotation is given.
  A recursive definition needs one. Inferring a body is a second typing pass, so compile time grows
  with the number of unannotated definitions that call each other.
- A parameter type inferred from a body call (Phase D) needs one overload with that name and arity.
  Overloaded methods such as `Console.WriteLine` need an annotation.
- A bare `fn` cannot fix a type argument that appears only as a whole parameter `!!n`, and a
  delegate class argument that is an unbound `!!n` cannot be inferred.
- A read of a later module value through a function call from an initializer is not caught.
- Member reads are one dot deep. `x.Name` and `Type.Name` work bare, but `a.b.c` is an unbound
  identifier. Write `(~> a .b .c)` instead. Dotted heads inside a threaded clause, such as
  `(~> x (xs.Thing))`, are not supported and have no tests; use the bare `.Thing` clause form.
- Racket's default error display crashes with `fprintf: ill-formed pattern string` when a message's
  source path contains `~`, for example an 8.3 short name in a temporary path such as `PEZ~1`. Plain
  Racket 8.16 does the same with `raise-syntax-error`, so the fault is in Racket's error display and
  Tui cannot fix it here. Work around it by setting `TEMP` and `TMP` to a path with no `~`, or by
  building from such a path.
- Type patterns (section 12.1) see only the members Tui models. `(open Gen)` checks the name and
  that there are type arguments, not the arity. `(or ...)` does not check that its branches bind the
  same `?id`s. `szarray` patterns compile but no example exercises them.
- Member lookup shows only the public members whose signatures use types Tui models. Arrays, structs
  and enums are modeled. Tui cannot define an enum, and it cannot write a static field that is not a
  constant. Multi-dimensional arrays, pointers and byrefs are not supported.
- Constant fields of types Tui cannot emit, such as `Byte`, are not read, so they are missing from
  completion and an unknown member error reports them.
- Array covariance is not modeled. An array of a derived type does not convert to an array of its base
  type.
- A named pattern exported from one module and used from another has not been tested.
- `try` is statement-only. A `try` inside an argument, operand, `new` or `set!` value is rejected, because the IL `.try` block needs an empty evaluation stack. Hoist it into a `let` to use its value.
- A `catch` cannot name an exception type defined in the assembly being built, or a generic type. Use a library exception type.
- A `finally` body must be void. It cannot produce a value.
- `never` cannot be written as an annotation, so a `define` that always throws must declare its real return type.
- Captured cells use one array per variable. Grouping captures into one object would reduce
  allocations; see [captures.md](captures.md) section 9.
