# Tui

Documentation: [docs/architecture.md](docs/architecture.md) explains how the compiler works, [docs/captures.md](docs/captures.md) covers closure captures, and [AGENTS.md](AGENTS.md) is the guide for agents working in this repository.

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
* `(fn DELEGATE (param ...) body ...)` builds a delegate instance, such as `Action<T>`, `Func<...>` or `Predicate<T>`. DELEGATE is written in full, or may be left out in four cases: when the `fn` is in a position that expects a delegate (for example the tail of a `define` whose return type is a delegate); when it is a bare argument of a .NET method or constructor (the overload supplies the delegate type, so `(.ForEach xs (fn (x) (Console.WriteLine x)))` works, and a generic method's type arguments that only the `fn` mentions are fixed by its body, as in `(Enumerable.Sum (Enumerable.Select xs (fn (x) (* x 2))))`); when every parameter is annotated, as in `(fn ([x : int]) (= (% x 2) 0))`, and the delegate is `Func` or `Action` of the body's type; or when an unannotated parameter is an argument of a call in the body whose name has exactly one overload, as in `(fn (s) (String.IsNullOrEmpty s))`. The parameter and return types come from the delegate's `Invoke` method. The arity must match and the body must produce the return type. Multi-argument generics need pipes because the reader splits `Func<int32,int32>` at the comma, so write `|Func<int32, int32>|`. Each lambda becomes a generated class with one field per captured cell and an `Invoke` method. The value is a delegate over `Invoke` on an instance of that class. Lambdas are first-class objects: they can be stored, passed, returned and invoked with `.Invoke`. A lambda can use the locals and arguments of the enclosing method, and of any enclosing lambda. A capture is shared, not copied: `set!` in the lambda is seen by the method and by other lambdas that capture the same variable. Each run of a `let` makes new variables, so a lambda in a loop body captures its own copy. Captures are stored in cells (a one-element array per variable), and the cell is copied into the class field when the lambda is created. The compiler decides which names are captured with two expansion passes. The first pass expands the method and records the variables that lambdas read. The second pass expands it again with those variables as cells. So a name a macro introduces into a lambda is captured like any other name. Methods with lambdas are expanded twice, so expansion must be deterministic. See [architecture.md](docs/architecture.md) sections 6 and 10 for how types are inferred.
* Overloads are resolved at expansion time. The implicit conversions are widening (int32 to int64, integers to float64), boxing to `object`, and reference upcasts to a base class or interface (no IL is emitted). An upcast costs less than a conversion to `object`, so `List<string>` picks `IEnumerable<string>` over `object`. Generic methods take explicit type arguments, e.g. `Enumerable.Where<int32>`. Inference only applies when a parameter's type is exactly the method's type variable, with no unification. Names with commas need pipes, e.g. `|Enumerable.Select<int32, int32>|`.
* A `fn` value can be passed as an argument and stored with `let`, like any other value.
* Module-level values: `(define name : type expr)` declares a static field on the module class. The type annotation is required. Each value gets a static initializer method, and the entry point runs the initializers in source order before the body. A function may read any module value. An initializer may read only the values defined before it: a direct read of a later value is a syntax error (`module value used before it is defined`). A read through a function call is not checked. A lambda that reads a module value needs no capture, because the value is a static field, and it sees the current value. `set!` on a module value writes the field. Module values can only be defined in a module, not in a REPL interaction. `examples/values.rkt` shows this.
* Macros are hygienic. `define-syntax-rule` is the simple form. `define-syntax-parser` (from `syntax/parse/define`) can ask the type checker about an argument and read the CLR metadata behind it. `examples/lists.rkt` has `sum-of`, which reads the element type from the closed `List<T>`'s `get_Item` return type, and raises a syntax error for element types it does not support. `examples/types.rkt` has `default-of`, which takes a type name: it expands to a literal for `int32`, `bool`, `float64` and `string`, or to `(new T)` for a class with a public parameterless constructor, and is a syntax error otherwise. `type-name-of` expands to the Tui name of its argument's type as a string literal; the argument is type checked but not evaluated. The phase-1 helpers `tui-type-of`, `tui-member-return-type`, `tui-resolve-type`, `tui-has-default-ctor?`, `tui-type->string` and `class-of` are exported `for-syntax`. `tui-static-overloads` and `tui-instance-overloads` list the overloads of a method name as type signatures, and `tui-member-names` lists a type's public method names. `supertypes` gives the transitive base classes and interfaces of a type. These only cover members whose signatures use types Tui models, so the overload counts are smaller than the full .NET counts. `examples/phase1.rkt` uses them.
* Syntax classes give macros the types of their arguments without calling those helpers. `typedExpr` matches any expression and has `x.Type`, the type the checker gives it. `typedList` is a `typedExpr` that also has `x.Elem`, the `get_Item` return type, or `#f`. `typeName` matches an identifier naming a type and has `x.Type`. Dot attributes are read in templates as `x.Type` and elsewhere as `(attribute x.Type)`. The classes live in a `begin-for-syntax` beside the helpers they call, because their attribute expressions run at phase 1.
* Partly done: the checker stamps each expanded fragment with its type as the `'tui-type` syntax property, and reads it back when it builds larger expressions. Macros do not yet read this property from their source arguments; the syntax classes above are still the way to get an argument's type.

## Running

* `racket examples/basics.rkt` runs the program. The module's `main` submodule assembles it into a temporary exe and runs it.
* Run the module in DrRacket. Each REPL interaction is type checked against the module's functions, compiled into a fresh entry point, assembled with the module's methods, and run. The REPL also accepts function `define`, `using`, `open` and macros. It rejects module value `define`.
* `racket -l- tui/build examples/basics.rkt build/basics.exe` writes a standalone exe.
* `raco test tests/` runs the end-to-end tests.
* `examples/linq.rkt` uses `System.Linq` on lists and stored lambdas. `examples/fnlib.rkt` is a library with the `compose` macro and the `square` identifier macro, which it type checks on its arguments. `examples/phase1.rkt` shows macros that read CLR metadata.

## Layout

| File                | Phase | Role                                                                               |
|---------------------|-------|------------------------------------------------------------------------------------|
| `main.rkt`          | 0, 1  | The language: `#%module-begin`, `#%top-interaction`, and the typed forms            |
| `private/clr.rkt`   | 1     | The .NET view: references, type names, methods, overloads, conversions (Rangi reader) |
| `private/runtime.rkt` | 0   | Turns a Tui program into Rangi ilasm, writes the exe, runs it                        |
| `build.rkt`         | 0     | Command line: compile a Tui module to an exe                                        |
