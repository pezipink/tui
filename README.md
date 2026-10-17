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
* Forms: `define`, `let`, `set!`, `begin`, `if`, `while`, `and`, `or`, `not`, `new`, `list-of`, the arithmetic operators `+ - * / %` (`+` also joins strings), the comparisons `< > <= >=`, and `=`.
* `(list-of e ...)` builds a `System.Collections.Generic.List<T>`. T is the type of the first element and every other element must convert to it. `(list-of 1 "a")` is a type error. An empty list needs `(new System.Collections.Generic.List<T>)`.
* Overloads are resolved at expansion time. The implicit conversions are widening (int32 to int64, integers to float64), boxing to `object`, and reference types to `object`.
* Macros are ordinary hygienic `define-syntax-rule` macros.

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
