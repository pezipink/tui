# Tui closure captures: design notes

Covers how `fn` lambdas capture locals and arguments, what the runtime shape is, and how the
capture logic interacts with the Racket expander. Code is in `main.rkt`. Tests are in
`tests\tui-test.rkt`. Examples are `examples\captures.rkt` and `examples\delegates.rkt`. Status at
the time of writing: 18/18 Tui tests pass, both examples pass PEVerify, and the full Rangi suite
passes (1660 tests).

## 1. User-visible semantics

- `(fn Delegate (p ...) body ...+)` builds a delegate. The body may read and write any local or
  argument of the enclosing method (or of an enclosing lambda). Those variables are captured.
- Captures are by reference. A `set!` in a lambda is seen by the method and by every other lambda
  that captured the same variable. Two lambdas made in the same `let` run share the variable.
- Each run of a `let` creates new variables. A lambda created in a loop body captures that
  iteration's copy. A lambda created before a later run of the same `let` keeps the old copy.
- A captured method argument is copied into a fresh cell when the method starts. After that, the
  method and its lambdas share the cell, not the original argument slot.
- Lambdas nest to any depth. An inner lambda reaches variables from every enclosing method or
  lambda through its parent's environment.
- Module-level `define`s have no enclosing locals, so only their own arguments and `let`s can be
  captured.
- REPL: each interaction is its own method (`__tui_main`, or a `define`). A lambda cannot capture a
  `let` from an earlier interaction, since those variables are not in scope.

## 2. Runtime shape (CLR)

For `(fn Func<int32,int32> (n) body)` with captured variables `k1..kN` (in capture order):

- The lambda body is a static method on `Program`, named `__lambdaN`. Its signature is
  `(object[] env, P1, ..., Pn) -> R`. The first argument is the env array, and the Invoke
  parameters follow it.
- A cell is a one-element array, `(szarray T)`, for a variable of type `T`. Reads are
  `ldelem T` at index 0 and writes are `stelem T` at index 0.
- The env is `object[]` with one entry per captured cell. The enclosing method fills it when the
  `fn` expression is evaluated: `ldc.i4 N; newarr object; (dup; ldc.i4 k; ldloc cell_k; stelem object)*`.
- The delegate is closed over the env: `ldftn __lambdaN` (signature includes the env type first),
  then `newobj Delegate::.ctor(object, native int)`. This is an ordinary, first-class delegate
  object. It can be stored, passed, returned and invoked with `.Invoke`.
- Inside the lambda's prologue, each env entry is copied into a local with
  `ldarg.0; ldc.i4 k; ldelem object; castclass (szarray T); stloc`. The body then reads the cell
  from that local like any other cell.
- Shared state is the cell array itself, not the env slot. The env stores references to the same
  arrays as the method, so writes through either side are visible to both.
- Reference-type cells (for example `string`) are verified by PEVerify (`examples\captures.rkt`).
  Other reference types are expected to work through the same TypeSpec path but are not yet
  verified.

## 3. Compile-time data structures (phase 1)

All of this lives in `(begin-for-syntax ...)` in `main.rkt`.

- `mctx` (`locals labels captured parent env`): per-method compile state.
  - `locals`: local types in slot order (may contain `cell-of`).
  - `labels`: counter for branch labels.
  - `captured`: hasheq of names mentioned in `fn` forms in this method's source. Built once by
    `captured-names` (see section 4).
  - `parent`: the enclosing `mctx` for a lambda, `#f` for a method or `__tui_main`.
  - `env`: list of `(tui-var . local-index)` for variables this lambda reads from its env array.
    Grows during compilation of the lambda body.
- `cell-of` (`type`): a local whose value is a cell array. `local-il` emits `(szarray T)` for it.
- `tui-var` (`kind index type owner`): a variable binding, stored as a syntax transformer so `set!`
  can find it with `syntax-local-value`.
  - `kind 'arg`: argument slot `index` of `owner`'s method.
  - `kind 'local`: plain local slot `index` of `owner`'s method.
  - `kind 'cell`: `index` is the local of `owner` that holds the cell array. The value is element 0.
  - `owner`: the `mctx` that created the binding. It is the identity check for plain variables.
- `current-mctx` (parameter): the method currently being expanded. `the-mctx` raises a syntax error
  when no method is active.
- `lifted-methods` (box): `(name . method-datum)` pairs waiting to be added to the program.
- `lambda-count` (box): counter for `__lambdaN` names. Reset in `tui-module-begin`.

Access is always through `load-var` and `store-var`. Do not emit `ldloc`/`ldarg` for a `'cell`
variable directly.

## 4. The pre-scan (`captured-names`) and macros

Problem: the environment must know, before compiling a method body, which of its variables a
lambda will need. Cells must exist at declaration time, because a cell is created when the
variable is bound (`let`) or when the method starts (argument). Compile-time discovery would come
too late.

Solution: a name-based syntactic pre-scan over the unexpanded method body.

- `captured-names forms` walks the raw syntax. For each pair whose head is `free-identifier=?`
  to `#'tui-fn`, it marks every identifier in the rest of the form, including nested `fn`s. The
  result is a hasheq of symbols.
- `new-mctx` stores that hash in `mctx-captured`. `compile-body` uses it for arguments, and
  `tui-let` uses it for bindings.
- Matching is by symbol, not by scope. A name is a cell if any `fn` in the method mentions it,
  even if that `fn` refers to a different binding. The cost is an unnecessary cell, not
  incorrect code. Scope is decided at expansion time by `syntax-local-value`.

Consequences for macro authors:

1. Macros whose arguments reach the `fn` body work. The argument identifiers are in the source
   syntax, so the pre-scan finds them. Example: `(my-mac k (fn ... k))` where `my-mac` expands to
   `(fn ... k)`.
2. Identifiers that a macro template introduces are not in the source, so they are missed. If a
   template creates `(let ([n 1]) (fn ... n))`, the `let` is compiled with a plain local, and the
   `fn` then fails with `a lambda can only capture n if its enclosing method names it in a fn form`.
   The error is raised at the `n` reference, which may be confusing. It is a known limitation.
   Possible fixes (not implemented):
   - Expand the method body once to collect `fn` references by binding, then compile with that
     information. This costs an extra expansion pass.
   - Make every `let` and argument a cell when the body contains any macro use. This is simpler but
     slower.
3. A `fn` whose head is produced by a macro is not seen by the pre-scan at all. Its free
   variables are still in the same situation as in point 2.

## 5. How this interacts with the expander

The compiler runs during expansion. Every Tui form is expanded with `local-expand` and a stop list
of `tui-il`. Each stop form carries `(type instructions)` as its payload, and `tc` reads it back.
So the IL is produced as a side effect of macro expansion.

Sequence for `(define (f [k : int]) : |Func<int32,int32>| (fn |Func<int32,int32>| (n) (+ n k)))`:

1. `tui-module-begin` resets `lifted-methods` and `lambda-count`. It emits one `tui-method` form
   per `define`, plus `tui-main` for the top-level expressions.
2. `tui-method` calls `compile-function`. `compile-body` creates an `mctx` for `f` with the
   pre-scan result, and parameterizes `current-mctx` to it.
3. `k` is an argument that `f`'s `captured` hash marks. `compile-body` allocates a cell local,
   copies `k` into it, and binds `k` to a `'cell` `tui-var`.
4. The body `(fn ...)` expands, and `tui-fn` calls `compile-lambda`. That function does four
   things:
   a. `delegate-signature` reads the `Invoke` method from the delegate type (`find-instance-methods`).
   b. It calls `compile-body` for the lambda with `parent` = `f`'s `mctx`. This is recursive
      expansion, and `current-mctx` is rebound to the lambda's `mctx`.
   c. It pushes `(__lambdaN . method)` onto `lifted-methods`.
   d. It builds the env in `f`'s frame: `cell-slot-in f k` returns `k`'s cell local.
5. `(n) (+ n k)` in the lambda: `k` is a `'cell` owned by `f`. `load-var` calls `cell-slot-in`
   with the lambda's `mctx`. That is not the owner, and it is not in the env yet, so it recurses
   to the parent (`f`, which owns it), then adds `(k . j)` to the lambda's env and allocates a
   local `j`. The prologue then copies env entry `k` into `j`.
6. `compile-body` for the lambda builds its env-code from `mctx-env` after the body is compiled.
   Compiling the body is what fills the env, so this ordering is required.
7. `tui-let` for a captured name also creates a cell when it runs. It creates it fresh, so each
   loop iteration gets its own cell.

Lifting the lambda bodies into the program:

- The `tui-lifting` macro wraps the program expression in `(let ([p e]) (tui-lifts-into p) p)`.
  `tui-lifts-into` emits `tui-program-add-method!` for every pending lambda. It must run after `e`
  has been expanded, because `e` is what fills `lifted-methods`.
- That is why it must be in a `let` body. Racket expands the RHS before the body, so the lambdas
  exist by the time the body macro runs. An earlier attempt with `(begin a (tui-lifts-into p))`
  did not work.
- REPL: `tui-top-interaction` resets `lifted-methods`, compiles the interaction, and then calls
  `tui-lifts-into`. This is what makes lambdas defined in one interaction callable from later ones.

IL is quoted: `tui-method` and `tui-main` emit `(quote <datum>)`, so the IL becomes a runtime
s-expression. `make-tui-program` assembles it with Rangi at phase 0 when the module runs.

## 6. Rangi dependency

Cell arrays and env entries use constructed element types, such as `(szarray (class ...))` or
`(szarray int32)`. Rangi `lower.rkt` previously rejected these as operands of `newarr`, `ldelem`,
`stelem`, `box`, `unbox`, `unbox.any`, `castclass` and `isinst`. Commit `b8a9084` now encodes
constructed operands as TypeSpec tokens (`constructed-type?`). Primitive operands keep their old
encoding. `tests\cli\array-lower-test.rkt` checks the TypeSpec form.

## 7. Invariants for future edits

- Compile the lambda body (`compile-body`) before building its env code. The env entries are only
  known after that.
- Compile a nested lambda inside its parent's `tc/bind`, so the parent's env is extended before the
  parent emits its own env code.
- Keep `mctx-env` order stable. The env array index is the order of addition, and `compile-lambda`
  and the prologue must agree.
- Every variable access goes through `load-var` / `store-var`. A plain variable whose owner is not
  the current `mctx` is an error, not a silent miscompile.
- `lambda-count` and `lifted-methods` are phase-1 state. Reset them in `tui-module-begin` and in
  the REPL path.
- Do not compare `tui-var`s by name. Use `eq?` identity (`assq` in `cell-slot-in`), because the same
  binding can appear under different identifiers after macro expansion.

## 8. Tests and examples

- `tests\tui-test.rkt`:
  - `fn captures locals and arguments by reference`: runs `examples\captures.rkt` and checks the
    output. It also covers a captured argument and a `let` whose value is changed with `set!`.
  - `fn builds delegates for Action, Func and Predicate`: `examples\delegates.rkt`.
  - `fn checks its delegate type and body`: arity, delegate type, return type, unknown type.
  - `the REPL lifts lambdas defined in an interaction`: runs `(define (mk) ...)` and then calls it
    from a later interaction.
- `examples\captures.rkt` prints `12 15 101 42 tui 0 20`. It covers a shared counter (`bump`), a
  captured argument (`adder`), two-level nesting, an inner lambda that reads a parent argument, a
  `string` capture, and a per-iteration `let` inside a `while`.

Test gap: there is no test that a macro-introduced capture fails with the documented message, and
no test for a reference-type cell other than `string`.

## 9. Open issues

- Macro-introduced captures are missed by the pre-scan (section 4). This is the most important gap
  for the "macros build languages" direction. A proper fix probably needs a post-expansion
  analysis, or conservative cells for any `let` in a body that uses macros.
- `lambda-count` resets per module. Two modules loaded in one process could reuse `__lambdaN` names
  in the same assembly. Not tested.
- Generic methods are out of scope. `type-methods` in `private\clr.rkt` excludes them, so generic
  methods such as LINQ calls are not callable. MethodSpec rows are written by Rangi (656 on
  mscorlib, matching SRM), so this is a Tui type-layer change, not a writer change.
- Each cell is one array per variable. A group of captured variables could share one env object,
  which would reduce allocations. Not needed yet.
- Design decisions were chosen as defaults: closed delegate over `object[]`, name-based pre-scan,
  shared mutable cells. The user committed the work without explicitly confirming them, so they
  should still be reviewed.
