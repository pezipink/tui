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
| `private/clr.rkt` | The CLR bridge: reference configuration, type datums, metadata lookup, `coercion`, overload and generic-method resolution. |
| `private/runtime.rkt` | Program assembly: `make-tui-program`, `tui-program->il`, `tui-write-exe`, `run-tui-program`. |
| `tests/tui-test.rkt` | The test suite (29 tests, about 4 minutes). |
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
   - `passthrough` is a Racket macro definition or `require`/`provide`, emitted unchanged.
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
- A function's type comes from its declaration (`fun-header`, `fun-declaration`).

A macro that builds `(tui-il ...)` by hand bypasses `il`, so it has no `'tui-type` property. In
that case `tc` raises `internal: untyped fragment`. Build fragments with `il` or through ordinary
Tui forms.

## 5. Type representation

A Tui type is a datum:

- A primitive symbol: `void`, `bool`, `char`, `int32`, `int64`, `float64`, `string`, `object`.
- `(class asm ns name args)` for a CLR class, with `args` the closed type arguments.
- `(szarray t)` for a single-dimension array, and `(mvar n)` for the nth type parameter of a
  generic method, which exists only before instantiation.

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
| Expected type | `'tui-expected` (`tc/expect`) | A `fn` with no delegate name (section 10) |
| Declaration | annotations, `fun-header`, module `define` | Parameters, return types, module values |
| CLR metadata | `clr-method` from `find-static-methods` and `find-instance-methods` | Overload candidates, parameter types, delegate `Invoke` arity |
| Bare fn placeholder | type `(lambda n)` and struct `deferred-fn` | A bare `fn` argument before its overload is chosen |

### CLR calls

A call to a .NET method (`compile-static-call`, `compile-instance-call`, `tui-new`) runs in three steps.

1. **Type the arguments** (`tc-args`). Each argument is expanded and typed. A bare `fn` is detected
   after `local-expand` by its `tui-fn` form. It gets the type `(lambda n)`, where n is its arity,
   and is kept as a `deferred-fn`.
2. **Choose the overload** (`resolve-overload`). Each candidate is closed by `infer-targs`, and each
   argument is scored with `arg-coercion`. A placeholder matches any delegate whose `Invoke` has n
   parameters, at cost 0. The lowest total cost wins. When costs tie, a non-generic candidate beats a
   generic one, as in C#. Groups are tried in order: a derived type's methods come before its bases',
   and the first group with an applicable candidate wins. A tie that remains is an error.
3. **Finish the call** (`finish-call`). If every type argument is fixed, each argument is converted to
   its parameter type (`finish-args`), and each deferred `fn` is compiled against that type with
   `tc/expect`. If a type argument is still open, the bare `fn`s fix it first (`compile-partial-fn`),
   then the arguments are converted. An argument whose type is still open is an error.

### Fixing type arguments (`infer-targs`)

Each parameter is matched against its argument, in order:

- A parameter that is exactly `(mvar n)` fixes `!!n` to the argument's type. Every use of `!!n` must
  agree (`bind-type`).
- A generic instance parameter, such as `IEnumerable<!!0>`, matches the argument's supertype with the
  same generic definition (`matching-supertype`). A `List<int32>` argument fixes `!!0` to `int32`.
- A placeholder binds nothing. A type argument that occurs only in bare-fn parameters stays open
  (`lambda-only?`). The fn's body fixes it in `finish-call`.
- Explicit type arguments (`Name<A>`) are used as given. Their count must match the method's arity.

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
| value type to `object` | 2 | `box` |

Reference upcasts emit no IL, because the verifier accepts a reference to a base type. The cost of
`object` is higher than any base class or interface, so `List<T>` prefers `IEnumerable<T>` over
`object`.

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
```

The type annotation is required. Each value gets:

- a static field `name`, declared with `.field (public static)`;
- a static method `__init_name` that computes `expr`.

The entry point runs the initializers in source order before the body, using
`value-inits`. Each initializer assigns its field with `stsfld`.

Rules:

- A function may read any module value.
- An initializer may read only the values defined before it. `value-scope` holds the names defined
  so far, and `check-global-scope` raises `module value used before it is defined` on a direct read
  of a later value. Reads through a function call are not checked, so `(define a : int (f))` where
  `f` reads a later value is not caught.
- A lambda that reads a module value needs no capture, because the value is a static field, and it
  sees the current value.
- `set!` on a module value writes the field (`tui-set!` handles `tui-global`).
- A module value can be defined only in a module. A REPL interaction that defines one raises
  `module values can only be defined in a module`.

The module-level machinery is `make-tui-global` (a `tui-global` struct bound with `define-syntax`),
`value-parts` (builds the field and initializer), the `tui-value-decls` macro (returns the field and
initializer pairs for the method list), and `compile-main`, which takes the initializers. The field
access form is `(ldsfld Program name type)`. Section 11 explains `Program`.

## 9. Two-pass expansion and lambda lifting

A method that contains a lambda is expanded twice. The first pass records the variables that
lambdas read. The second pass expands the method again with those variables as cells. `run-passes`
drives this. `current-pass` holds the pass's data, including `pass-cells`.

Consequences:

- Expansion must be deterministic. A macro that reads mutable phase-1 state can break the second
  pass.
- A name a macro introduces into a lambda is captured like any other name.

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
  `.Method recv` with a variable receiver. The scan skips nested binders, so it does not look
  inside `fn`, `let`, `let*`, `letrec` or `define`. Example: `(fn (s) (String.IsNullOrEmpty s))`.
  Overloaded methods such as `Console.WriteLine` give no inference, so the parameter must be
  annotated.

- **From a generic call's body** (Phase E): a bare `fn` argument can fix a method type argument that
  no other argument mentions, such as `TResult` in `Enumerable.Select`. The `fn`'s parameters take
  the delegate's already-known types, and its body type fixes the rest. Section 6 describes the rule
  and its limits.

A lambda is a value. It can be stored with `let`, passed as an argument, returned, and invoked with
`.Invoke`. A delegate is called with `.Invoke`, as in `(.Invoke f x)`. Writing `(f x)` for a delegate `f` is an
error. Functions are called directly, which emits `call` on `Program`. A module value applied as
`(g x)` is rejected with `module values are not first-class in Tui; use the name`.

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

Syntax classes for macro arguments: `typedExpr` (`x.Type`), `typedList` (`x.Type`, `x.Elem`),
and `typeName` (`x.Type`).

Limits: the helpers see only members whose signatures use types Tui models, so overload counts are
smaller than the full .NET counts.

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
- `expected (define name : type expr)`
- `expected (define (name [param : type] ...) : type body ...+)`
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

## 15. Tests

`tests/tui-test.rkt` contains the test helpers:

- `run-example file` runs the `main` submodule of an example and returns its output lines.
- `run-tui form ...` expands `(module m tui form ...)` in a fresh namespace and returns its output.
- `tui-error form ...` returns the syntax error message, or `#f` if there is none.

The REPL tests evaluate `(#%top-interaction . form)` in the module's namespace.

Run the suite with `raco test tests\tui-test.rkt`. It takes about 4 minutes. Run `raco make
main.rkt` first if you have changed `main.rkt` or `private/*.rkt`.

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
| `macros.rkt` | hygienic macros | `Tui macros are hygienic` |
| `types.rkt` | `default-of`, `type-name-of` | `default-of takes a type name ...`, `type-name-of expands ...` |
| `lists.rkt` | `list-of`, `sum-of` | `the list-of example runs`, `sum-of reads the element type ...` |
| `linq.rkt` | LINQ over stored lambdas, reference conversions | `LINQ works with stored lambdas, library macros and reference conversions` |
| `fnlib.rkt` | `compose` and `square` library macros | `compose checks that both arguments are Func<int32, int32>` |
| `phase1.rkt` | metadata helpers at expansion time | `phase 1 macros read overloads, supertypes and members from metadata` |
| `values.rkt` | module values, `set!`, initialization order | `module values are static fields set in source order, and set! updates them` |

## 17. Known limits

- Inference is limited to exact `(mvar n)` parameters, base-type matches of generic instance
  parameters, and the bare `fn` body rule. There is no unification.
- A parameter type inferred from a body call (Phase D) needs one overload with that name and arity.
  Overloaded methods such as `Console.WriteLine` need an annotation.
- A bare `fn` cannot fix a type argument that appears only as a whole parameter `!!n`, and a
  delegate class argument that is an unbound `!!n` cannot be inferred.
- A read of a later module value through a function call from an initializer is not caught.
- A delegate cannot be called as `(f x)`. Use `.Invoke`.
- `.Count` is rejected; write `get_Count` or use the property getter form.
- Error display crashes with `fprintf: ill-formed pattern string` when a message contains `~`, for
  example an 8.3 temporary path. This is a pre-existing bug and is not fixed here.
- Captured cells use one array per variable. Grouping captures into one object would reduce
  allocations; see [captures.md](captures.md) section 9.
