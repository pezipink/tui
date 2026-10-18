# Tui closure captures: design notes

Covers how `fn` lambdas capture locals and arguments, what the runtime shape is, and how the
capture logic interacts with the Racket expander. Code is in `main.rkt`. Tests are in
`tests\tui-test.rkt`. Examples are `examples\captures.rkt` and `examples\delegates.rkt`. Status at
the time of writing: 22/22 Tui tests pass. All six examples pass PEVerify after the move to
generated classes and the two-pass discovery.

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

- Each lambda is a generated top-level class, named `closureN` (from `gensym`). Its shape is:
  - One public field per captured cell, `capK : (szarray T)`.
  - A public parameterless constructor that calls `System.Object::.ctor`.
  - One public instance method `Invoke` with the delegate's parameters. Its return type is R.
  The lambda body is the `Invoke` method, so `this` is argument 0 and the Invoke parameters start
  at argument 1.
- A cell is a one-element array, `(szarray T)`, for a variable of type `T`. Reads are
  `ldelem T` at index 0 and writes are `stelem T` at index 0.
- At the `fn` expression, the enclosing method does `newobj closureN ()`, then for each captured
  cell does `dup; ldloc cell_k; stfld closureN capK (szarray T)`. It then does
  `ldftn R closureN::Invoke (P...)` and `newobj Delegate::.ctor(object, native int)`. The delegate
  target is the new object. This is an ordinary, first-class delegate object. It can be stored,
  passed, returned and called as `(f x)` or `(.Invoke f x)`.
- Inside `Invoke`, each field is copied into a local with `ldarg.0; ldfld closureN capK (szarray T);
  stloc`. The body then reads the cell from that local like any other cell.
- Shared state is the cell array itself, not the field. The field holds a reference to the same
  array as the method, so writes through either side are visible to both.
- Generated classes are top-level siblings of `<Module>`. Rangi rejects nested classes inside
  `<Module>`, so they cannot be nested there.
- Reference-type cells (for example `string`) are verified by PEVerify (`examples\captures.rkt`).
  Other reference types are expected to work through the same TypeSpec path but are not yet
  verified.

## 3. Compile-time data structures (phase 1)

All of this lives in `(begin-for-syntax ...)` in `main.rkt`.

- `mctx` (`locals labels parent env`): per-method compile state.
  - `locals`: local types in slot order (may contain `cell-of`).
  - `labels`: counter for branch labels.
  - `parent`: the enclosing `mctx` for a lambda, `#f` for a method or `__tui_main`.
  - `env`: list of `(tui-var . local-index)` for variables this lambda reads from its class
    fields. Grows during compilation of the lambda body.
- `pass` (`discovering? cells used next lambdas`): state for one of the two compile passes of a
  top-level method (section 4). `cells` is the set of ordinals that must be cells, `used` collects
  the ordinals a lambda reads, `next` is the ordinal counter, and `lambdas` counts lambdas seen.
- `cell-of` (`type`): a local whose value is a cell array. `local-il` emits `(szarray T)` for it.
- `tui-var` (`kind index type owner ord`): a variable binding, stored as a syntax transformer so
  `set!` can find it with `syntax-local-value`.
  - `kind 'arg`: argument slot `index` of `owner`'s method.
  - `kind 'local`: plain local slot `index` of `owner`'s method.
  - `kind 'cell`: `index` is the local of `owner` that holds the cell array. The value is element 0.
  - `owner`: the `mctx` that created the binding. It is the identity check for plain variables.
  - `ord`: the binding's ordinal (section 4).
- `current-mctx` (parameter): the method currently being expanded. `the-mctx` raises a syntax error
  when no method is active.
- `lifted-classes` (box): the identifiers returned by `syntax-local-lift-expression` for each
  generated class datum, waiting to be added to the program. Reset in `tui-module-begin` and in
  `tui-top-interaction`.

Access is always through `load-var` and `store-var`. Do not emit `ldloc`/`ldarg` for a `'cell`
variable directly.

## 4. Deciding which variables are cells (two passes)

A cell must exist when its variable is bound, and a `let` or argument is bound before the `fn`
that reads it is reached. So the compiler needs to know the captured variables before it emits
the binding. It gets that from a discovery pass, not from a syntactic scan.

- `run-passes` compiles one top-level method body (`compile-function`, `compile-main`) in two
  passes. The discovery pass compiles with every variable in a plain slot. When a lambda reads a
  variable from another method, `plain-slot` records that variable's ordinal in `used` and returns
  a dummy index. The code from this pass is discarded if it has any lambdas.
- An ordinal is assigned to each binding (argument or `let` variable) by `next-ordinal!`, in the
  order that bindings are created. Both passes create bindings in the same order, so the same
  ordinal names the same variable in both. This holds because expansion is deterministic, and
  macro-introduced bindings are numbered like any other.
- If the body has no lambdas, the discovery output is used as is, so most methods are expanded
  once. Otherwise the second pass runs with `cells` = the discovery pass's `used`. A binding whose
  ordinal is in `cells` becomes a cell: `compile-body` for an argument, and `tui-let` for a `let`.
  Lambdas are lifted only in the second pass.
- Because the ordinal comes from the expansion itself, a macro-introduced binding is handled the
  same way as a written one. `(my-mac)` expanding to `(let ([n 1]) (fn ... n))` works, and there is
  a test for it.
- Cost: a method with a lambda is expanded twice. A method without one is expanded once.

Limitation: the discovery pass must produce the same bindings in the same order as the second
pass. A macro whose expansion depends on mutable phase-1 state (for example, a counter that changes
between expansions) would break this. None of the examples do this.

## 5. How this interacts with the expander

The compiler runs during expansion. Every Tui form is expanded with `local-expand` and a stop list
of `tui-il`. Each stop form carries `(type instructions)` as its payload, and `tc` reads it back.
So the IL is produced as a side effect of macro expansion.

Sequence for `(define (f [k : int]) : |Func<int32,int32>| (fn |Func<int32,int32>| (n) (+ n k)))`:

1. `tui-module-begin` resets `lifted-classes`. It emits one `tui-method` form per `define`, plus
   `tui-main` for the top-level expressions.
2. `tui-method` calls `compile-function`, which calls `run-passes`. The discovery pass compiles
   `f`'s body. `compile-body` gives `k` an ordinal and creates an `mctx` for `f`.
3. The body `(fn ...)` expands, and `tui-fn` calls `compile-lambda`. The discovery pass reads `k`
   inside the lambda, so `k`'s ordinal goes into `used`. Since the body has a lambda, the second
   pass runs. There `k` has a cell ordinal, so `compile-body` allocates a cell local, copies `k`
   into it, and binds `k` to a `'cell` `tui-var`.
4. In the second pass, `compile-lambda` does four things. (In the discovery pass it only runs
   step b, so it can see which variables the body reads, and returns a placeholder instead.)
   a. `delegate-signature` reads the `Invoke` method from the delegate type (`find-instance-methods`).
   b. It calls `compile-body` for the lambda with `parent` = `f`'s `mctx` and `owner` = the class
      name. This is recursive expansion, and `current-mctx` is rebound to the lambda's `mctx`.
   c. It builds the class datum (fields, constructor, `Invoke`). It lifts `(quote <datum>)` with
      `syntax-local-lift-expression`, and pushes the lifted identifier onto `lifted-classes`.
   d. It emits the instance creation at the `fn` site: `newobj`, one `stfld` per capture (the cell
      is `cell-slot-in f k`), then `ldftn` and the delegate `newobj`.
5. `(n) (+ n k)` in the lambda: `k` is a `'cell` owned by `f`. `load-var` calls `cell-slot-in`
   with the lambda's `mctx`. That is not the owner, and it is not in the env yet, so it recurses
   to the parent (`f`, which owns it), then adds `(k . j)` to the lambda's env and allocates a
   local `j`. The prologue then copies field `capK` into `j`.
6. `compile-body` for the lambda builds its env-code from `mctx-env` after the body is compiled.
   Compiling the body is what fills the env, so this ordering is required.
7. `tui-let` for a captured name also creates a cell when it runs. It creates it fresh, so each
   loop iteration gets its own cell.

Lifting the generated classes into the program:

- The `tui-lifting` macro wraps the program expression in `(let ([p e]) (tui-lifts-into p) p)`.
  `tui-lifts-into` emits `tui-program-add-class!` for every pending class. It must run after `e`
  has been expanded, because `e` is what fills `lifted-classes`.
- That is why it must be in a `let` body. Racket expands the RHS before the body, so the classes
  exist by the time the body macro runs. An earlier attempt with `(begin a (tui-lifts-into p))`
  did not work.
- REPL: `tui-top-interaction` resets `lifted-classes`, compiles the interaction, and then calls
  `tui-lifts-into`. This is what makes lambdas defined in one interaction callable from later ones.
- Naming deviation: the class name comes from `gensym`, not from `syntax-local-lift-expression`.
  The name is a symbol written into the IL at the `fn` site (`newobj`, `stfld`, `ldftn`), and the
  identifier a lift returns is not a symbol the IL can use. So the name is generated directly, and
  the lift only places the class datum in the module.

IL is quoted: `tui-method` and `tui-main` emit `(quote <datum>)`, so the IL becomes a runtime
s-expression. `make-tui-program` assembles it with Rangi at phase 0 when the module runs. Generated
classes are added before `<Module>`, by `tui-program->il`.

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
- Keep `mctx-env` order stable. The field index is the order of addition, and `compile-lambda`
  and the prologue must agree.
- Every variable access goes through `load-var` / `store-var`. A plain variable whose owner is not
  the current `mctx` is an error, not a silent miscompile.
- `lifted-classes` is phase-1 state. Reset it in `tui-module-begin` and in the REPL path.
- Do not compare `tui-var`s by name. Use `eq?` identity (`assq` in `cell-slot-in`), because the same
  binding can appear under different identifiers after macro expansion.
- The lift datum must be built with `quote-syntax` from `main.rkt`. A bare `quote` built in the
  caller's context is unbound in a Tui module, which exports no `quote`.

## 8. Tests and examples

- `tests\tui-test.rkt`:
  - `fn captures locals and arguments by reference`: runs `examples\captures.rkt` and checks the
    output. It also covers a captured argument and a `let` whose value is changed with `set!`.
  - `lambdas nest and share mutable captures through their generated classes`: a lambda inside a
    lambda that reads an outer argument, a `let` and its own argument; a `set!` through a
    delegate's class field; and two `let`s in one interaction.
  - `fn builds delegates for Action, Func and Predicate`: `examples\delegates.rkt`.
  - `fn checks its delegate type and body`: arity, delegate type, return type, unknown type.
  - `fn captures a variable that a macro introduces`: a macro whose template binds `n` and returns
    a `fn` that updates it. Two invocations of the delegate show the shared cell.
  - `the REPL lifts lambdas defined in an interaction`: runs `(define (mk) ...)` and then calls it
    from a later interaction, and captures a `let` inside one interaction.
- `examples\captures.rkt` prints `12 15 101 42 tui 0 20`. It covers a shared counter (`bump`), a
  captured argument (`adder`), two-level nesting, an inner lambda that reads a parent argument, a
  `string` capture, and a per-iteration `let` inside a `while`.

Test gap: there is no test for a reference-type cell other than `string`.

## 9. Open issues

- Each cell is one array per variable. A group of captured variables could share one object, which
  would reduce allocations. Not needed yet.
- The discovery pass requires deterministic expansion (section 4). Nothing in the repo breaks this,
  but a macro that reads mutable phase-1 state could.
- The user approved one generated class per lambda with cell fields, two-pass discovery for cells,
  and shared mutable cells at the design check-in. The open question is only the first bullet:
  whether captured variables should share one object.
- The lift only places the class datum in the module. The generated class name comes from
  `gensym`, because the IL needs a symbol and a lifted identifier cannot be used there. The
  `lifted-classes` box is still needed to collect the lifted identifiers.

