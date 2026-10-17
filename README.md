# Tui

Tui is a tiny prototype .NET language built on [Rangi](https://github.com/pezipink/rangi). It shows the two ways a Racket `#lang` can use Rangi:

* **At phase 1, as a type system.** While the macro expander expands a Tui module, the language reads the referenced assemblies (mscorlib and anything named with `using`) with Rangi's lazy CLI metadata reader. It uses them to resolve type names, find methods and overloads, and report type errors at expansion time, so DrRacket shows them as you edit.
* **At phase 0, as a back end.** Each Tui expression expands to typed IL. The module collects its methods into a Rangi ilasm program, which Rangi assembles into a Windows PE image and runs on the .NET Framework CLR.

Tui uses the s-expression reader, so there is no parser.

## Requirements

* Windows with the .NET Framework 4.x and the v4.7.1 reference assemblies (`C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.7.1\`).
* Racket 8.16 CS.
* Rangi installed as the `rangi` package, for example `raco pkg install --link --name rangi c:\repos\rangi`.

Install Tui itself with `raco pkg install --link --name tui c:\repos\tui`.

## The language

```racket
#lang tui
(using "System")                ; reference an assembly by name or by path
(open System.Text)              ; search a namespace for short type names

(define (fact [n : int]) : int
  (if (< n 2) 1 (* n (fact (- n 1)))))

(define-syntax-rule (unless c body ...) (if c (begin) (begin body ...)))

(Console.WriteLine (fact 10))                   ; static call: Type.Method
(Console.WriteLine (.ToUpper "kia ora"))        ; instance call: .Method receiver args
(let ([sb (new StringBuilder)])
  (.Append sb "ab")
  (Console.WriteLine (.ToString sb)))
```

* Types: `int`/`int32`, `long`/`int64`, `double`/`float64`, `bool`, `char`, `string`, `object`, `void`, and any public .NET class by name.
* Forms: `define`, `let`, `set!`, `begin`, `if`, `while`, `and`, `or`, `not`, `new`, `list-of`, `fn`, the arithmetic operators `+ - * / %` (`+` also joins strings), the comparisons `< > <= >=`, and `=`.
* `(list-of e ...)` builds a `System.Collections.Generic.List<T>`. T is the type of the first element and every other element must convert to it. `(list-of 1 "a")` is a type error. An empty list needs `(new System.Collections.Generic.List<T>)`.
* `(fn DELEGATE (param ...) body ...)` builds a delegate instance, such as `Action<T>`, `Func<...>` or `Predicate<T>`. DELEGATE is written in full, and the parameter and return types come from the delegate's `Invoke` method. The arity must match and the body must produce the return type. Multi-argument generics need pipes because the reader splits `Func<int32,int32>` at the comma, so write `|Func<int32, int32>|`. Each lambda becomes a static method named `__lambdaN` on Program, and the value is a delegate over that method. Lambdas are first-class objects: they can be stored, passed, returned and invoked with `.Invoke`. A lambda can use the locals and arguments of the enclosing method. A capture is shared, not copied: `set!` in the lambda is seen by the method and by other lambdas that capture the same variable. Each run of a `let` makes new variables, so a lambda in a loop body captures its own copy. Captures are stored in cells (a one-element array per variable), and the delegate is a static method that receives an `object[]` of cells as its first argument. The compiler decides which names are captured by scanning the `fn` forms in the enclosing method's source, so a name a macro introduces into a lambda without writing it in the `fn` form is not captured.
* Overloads are resolved at expansion time. The implicit conversions are widening (int32 to int64, integers to float64), boxing to `object`, and reference types to `object`.
* Macros are hygienic. `define-syntax-rule` is the simple form. `define-syntax-parser` (from `syntax/parse/define`) can ask the type checker about an argument and read the CLR metadata behind it. `examples/lists.rkt` has `sum-of`, which reads the element type from the closed `List<T>`'s `get_Item` return type, and raises a syntax error for element types it does not support. `examples/types.rkt` has `default-of`, which takes a type name: it expands to a literal for `int32`, `bool`, `float64` and `string`, or to `(new T)` for a class with a public parameterless constructor, and is a syntax error otherwise. `type-name-of` expands to the Tui name of its argument's type as a string literal; the argument is type checked but not evaluated. The phase-1 helpers `tui-type-of`, `tui-member-return-type`, `tui-resolve-type`, `tui-has-default-ctor?`, `tui-type->string` and `class-of` are exported `for-syntax`.
* Syntax classes give macros the types of their arguments without calling those helpers. `typedExpr` matches any expression and has `x.Type`, the type the checker gives it. `typedList` is a `typedExpr` that also has `x.Elem`, the `get_Item` return type, or `#f`. `typeName` matches an identifier naming a type and has `x.Type`. Dot attributes are read in templates as `x.Type` and elsewhere as `(attribute x.Type)`. The classes live in a `begin-for-syntax` beside the helpers they call, because their attribute expressions run at phase 1.
* Not done: attaching the type as a `syntax-property` on the expression, so a macro could read `#.x.Type` without a syntax class. This is deferred. The syntax classes already give the same surface for macro arguments. A property would also need the expander to set it on every expression, and would have to match the checker's types exactly.

## Running

* `racket examples/basics.rkt` runs the program. The module's `main` submodule assembles it into a temporary exe and runs it.
* Run the module in DrRacket. Each REPL interaction is type checked against the module's functions, compiled into a fresh entry point, assembled with the module's methods, and run. The REPL also accepts `define`, `using`, `open` and macros.
* `racket -l- tui/build examples/basics.rkt build/basics.exe` writes a standalone exe.
* `raco test tests/` runs the end-to-end tests.

## Layout

| File                | Phase | Role                                                                               |
|---------------------|-------|------------------------------------------------------------------------------------|
| `main.rkt`          | 0, 1  | The language: `#%module-begin`, `#%top-interaction`, and the typed forms            |
| `private/clr.rkt`   | 1     | The .NET view: references, type names, methods, overloads, conversions (Rangi reader) |
| `private/runtime.rkt` | 0   | Turns a Tui program into Rangi ilasm, writes the exe, runs it                        |
| `build.rkt`         | 0     | Command line: compile a Tui module to an exe                                        |
