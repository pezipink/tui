#lang racket/base
; End-to-end tests: Tui source is type checked by the expander, assembled with Rangi, and run on
; the CLR. Requires Windows with .NET Framework 4.x and its reference assemblies.
(require rackunit racket/file racket/port racket/runtime-path racket/string
         tui/private/runtime)

(define-runtime-path examples "../examples")

; The CLR writes CRLF line endings.
(define (lines s) (string-split (string-replace s "\r" "") "\n"))

(define (run-example file)
  (lines (with-output-to-string
           (λ () (dynamic-require `(submod ,(build-path examples file) main) #f)))))

; Expands and runs `(module m tui form ...)` in a fresh namespace; returns its output lines.
(define (run-tui . forms)
  (parameterize ([current-namespace (make-base-namespace)])
    (eval `(module m tui ,@forms))
    (lines (with-output-to-string (λ () (eval '(require (submod 'm main))))))))

; The syntax error message raised while expanding a Tui module, or #f if it expands.
(define (tui-error . forms)
  (parameterize ([current-namespace (make-base-namespace)])
    (with-handlers ([exn:fail:syntax? exn-message])
      (expand `(module m tui ,@forms))
      #f)))

(test-case "examples run"
  (check-equal? (run-example "basics.rkt")
                '("3628800" "832040" "KIA ORA!" "1.4142135623731" "5"))
  (define out (run-example "macros.rkt"))
  (check-equal? (list-ref out 14) "FizzBuzz")
  (check-equal? (list-tail out 15)
                '("ababab" "one is not two" "github.com" "/pezipink/rangi")))

(test-case "generic methods infer type arguments from argument types or take explicit ones"
  (check-equal?
   (run-tui '(let ([t (System.Tuple.Create 1 "a")])
               (Console.WriteLine (.get_Item1 t))
               (Console.WriteLine (.get_Item2 t))))
   '("1" "a"))
  (check-equal?
   (run-tui '(let ([t (|System.Tuple.Create<int, string>| 2 "b")])
               (Console.WriteLine (.get_Item1 t))))
   '("2"))
  (check-equal?
   (run-tui '(Console.WriteLine (.get_Length (System.Activator.CreateInstance<System.Text.StringBuilder>))))
   '("0"))
  (check-true
   (regexp-match? #rx"no overload"
                  (tui-error '(System.Tuple.Create<int> 1 2)))))

(test-case "generic instances call members on the open signature"
  (check-equal?
   (run-tui '(let ([l (new System.Collections.Generic.List<int>)])
               (.Add l 3)
               (.Add l 4)
               (Console.WriteLine (.get_Count l))
               (Console.WriteLine (.get_Item l 1))))
   '("2" "4")))

(test-case "list-of infers its element type from the first element"
  (check-equal?
   (run-tui '(let ([xs (list-of 1 2 3)])
               (Console.WriteLine (.get_Count xs))
               (Console.WriteLine (.get_Item xs 2))
               (.Add xs 4)
               (Console.WriteLine (.get_Count xs))))
   '("3" "3" "4"))
  (check-equal?
   (run-tui '(let ([names (list-of "a" "b")])
               (.Add names "c")
               (Console.WriteLine (.get_Item names 2))))
   '("c")))

(test-case "the list-of example runs"
  (check-equal? (run-example "lists.rkt")
                '("tui " "42" "is super" "58" " cool!!!" "3" "linus" "6" "pink squirrel labs" "24")))

(test-case "default-of takes a type name and expands to its default value"
  (check-equal? (run-example "types.rkt")
                '("0" "False" "0" "0" "0" "System.Collections.Generic.List<int32>")))

(test-case "type-name-of expands to the Tui name of its argument's type"
  (define types-path (path->string (build-path examples "types.rkt")))
  (check-equal?
   (run-tui `(require (file ,types-path))
            '(Console.WriteLine (type-name-of 1))
            '(Console.WriteLine (type-name-of (list-of 1 2))))
   '("int32" "System.Collections.Generic.List<int32>")))

(test-case "default-of rejects types whose default it cannot build"
  (define types-path (path->string (build-path examples "types.rkt")))
  (check-regexp-match #rx"default-of supports int32, bool, float64, string and classes .*, got int64"
                      (tui-error `(require (file ,types-path)) '(default-of int64)))
  (check-regexp-match #rx"got System.Console"
                      (tui-error `(require (file ,types-path)) '(default-of System.Console)))
  (check-regexp-match #rx"unknown type" (tui-error `(require (file ,types-path)) '(default-of Nope))))

(test-case "sum-of reads the element type from the closed List's metadata"
  (check-regexp-match #rx"sum-of needs a list of int32 or string elements, got bool"
                      (tui-error `(require (file ,(path->string (build-path examples "lists.rkt"))))
                                 '(sum-of (list-of #t))))
  (check-regexp-match #rx"sum-of needs a list .* got a non-list"
                      (tui-error `(require (file ,(path->string (build-path examples "lists.rkt"))))
                                 '(sum-of 1))))

(test-case "list-of rejects elements and calls that do not match its type"
  (check-regexp-match #rx"expected int32, got string" (tui-error '(list-of 1 "a")))
  (check-regexp-match #rx"no overload of .*Add accepts \\(int32\\)"
                      (tui-error '(let ([xs (list-of "a")]) (.Add xs 1))))
  (check-regexp-match #rx"list-of needs an element" (tui-error '(list-of))))

(test-case "the REPL runs each interaction against the module's functions"
  (define mod (build-path examples "basics.rkt"))
  (dynamic-require mod #f)
  (define ns (module->namespace mod))
  (define (repl form)
    (lines (with-output-to-string
             (λ () (parameterize ([current-namespace ns])
                     (eval (namespace-syntax-introduce
                            (datum->syntax #f (cons '#%top-interaction form)))))))))
  (check-equal? (repl '(fact 5)) '("120"))
  (check-equal? (repl '(< 1 2)) '("True"))
  (check-equal? (repl '(Console.WriteLine "side effect")) '("side effect"))
  (check-equal? (repl '(define (sq [x : int]) : int (* x x))) '())
  (check-equal? (repl '(sq (fact 4))) '("576"))
  (check-exn #rx"expected int32, got string"
             (λ () (repl '(sq "four")))))

(test-case "Tui macros are hygienic"
  (check-equal?
   (run-tui '(define-syntax-rule (swap! a b) (let ([tmp a]) (set! a b) (set! b tmp)))
            '(let ([tmp 1] [other 2])
               (swap! tmp other)
               (Console.WriteLine tmp)
               (Console.WriteLine other)))
   '("2" "1")))

(test-case "numbers, strings and booleans"
  (check-equal?
   (run-tui '(Console.WriteLine (+ 1 2.5))
            '(Console.WriteLine (- 10))
            '(Console.WriteLine (% 17 5))
            '(Console.WriteLine (+ "a" "b" "c"))
            '(Console.WriteLine (= "x" (+ "" "x")))
            '(Console.WriteLine (and #t (or #f (not #f))))
            '(Console.WriteLine (<= 3 3))
            '(Console.WriteLine 5000000000)
            '(Console.WriteLine (.ToString #\z)))
   '("3.5" "-10" "2" "abc" "True" "True" "True" "5000000000" "z")))

(test-case "type errors are reported at expansion time"
  (check-regexp-match #rx"returns int32 but its body has type string"
                      (tui-error '(define (f) : int "s")))
  (check-regexp-match #rx"expected bool, got int32" (tui-error '(if 1 2 3)))
  (check-regexp-match #rx"expected int32, got string"
                      (tui-error '(let ([x 1]) (set! x "s"))))
  (check-regexp-match #rx"no overload of Console.WriteLine accepts \\(int32 int32 int32\\)"
                      (tui-error '(Console.WriteLine 1 2 3)))
  (check-regexp-match #rx"unknown type Nope" (tui-error '(Nope.Foo)))
  (check-regexp-match #rx"unbound identifier" (tui-error '(Console.WriteLine y)))
  (check-regexp-match #rx"unknown type" (tui-error '(define (f [x : Widget]) x)))
  (check-false (tui-error '(Console.WriteLine "fine"))))

(test-case "output is deterministic"
  (define p (dynamic-require (build-path examples "basics.rkt") 'tui-program))
  ; the assembly name comes from the file name, so write both under the same name
  (define dir (make-temporary-directory "tui-det-~a"))
  (define pa (build-path dir "a" "same.exe"))
  (define pb (build-path dir "b" "same.exe"))
  (make-directory* (build-path dir "a"))
  (make-directory* (build-path dir "b"))
  (tui-write-exe p pa)
  (tui-write-exe p pb)
  (check-equal? (file->bytes pa) (file->bytes pb))
  (delete-directory/files dir))

(test-case "fn builds delegates for Action, Func and Predicate"
  (check-equal? (run-example "delegates.rkt")
                '("10" "20" "30" "1" "2" "42" "81" "6" "from fn" "no args")))

(test-case "fn checks its delegate type and body"
  (check-regexp-match #rx"fn has 0 parameters but .*Action<int32> takes 1"
                      (tui-error '(fn Action<int32> () (Console.WriteLine 1))))
  (check-regexp-match #rx"fn needs a delegate type, got System.Console"
                      (tui-error '(fn System.Console (x) x)))
  (check-regexp-match #rx"function returns int32 but its body has type string"
                      (tui-error '(fn |Func<int32, int32>| (n) "s")))
  (check-regexp-match #rx"unknown type" (tui-error '(fn Nope (x) x))))

(test-case "fn captures locals and arguments by reference"
  (check-equal? (run-example "captures.rkt")
                '("12" "15" "101" "42" "tui" "0" "20"))
  (check-equal? (run-tui '(define (f [k : int]) : |Func<int32, int32>|
                            (fn |Func<int32, int32>| (n) (+ n k)))
                         '(Console.WriteLine (.Invoke (f 4) 1)))
                '("5"))
  (check-equal? (run-tui '(let ([k 2])
                            (Console.WriteLine (.Invoke (fn Func<int32> () (* k 3))))
                            (set! k 5)
                            (Console.WriteLine (.Invoke (fn Func<int32> () k)))))
                '("6" "5")))

(test-case "fn without a delegate name takes the expected delegate type"
  (check-equal? (run-tui '(define (sq) : |Func<int32, int32>| (fn (n) (* n n)))
                         '(Console.WriteLine (.Invoke (sq) 9)))
                '("81"))
  (check-regexp-match #rx"needs an expected delegate type"
                      (tui-error '(let ([f (fn (n) n)]) f))))

(test-case "annotated fn parameters take Func or Action from the body type"
  (check-equal? (run-tui '(let ([even (fn ([x : int]) (= (% x 2) 0))])
                            (Console.WriteLine (.Invoke even 4))
                            (Console.WriteLine (.Invoke even 3))))
                '("True" "False"))
  (check-equal? (run-tui '(let ([k 3])
                            (let ([add (fn ([a : int] [b : int]) (+ (+ a b) k))])
                              (Console.WriteLine (.Invoke add 1 2)))))
                '("6"))
  (check-equal? (run-tui '(let ([say (fn ([s : string]) (Console.WriteLine s))])
                            (.Invoke say "hi")))
                '("hi"))
  (check-equal? (run-tui '(let ([hello (fn () (Console.WriteLine "hello"))])
                            (.Invoke hello)))
                '("hello"))
  (check-equal? (run-tui '(let ([five (fn () 5)])
                            (Console.WriteLine (.Invoke five))))
                '("5"))
  (check-regexp-match #rx"parameter n is string but .*Func<int32, int32> takes int32"
                      (tui-error '(let ([g (fn |Func<int32, int32>| ([n : string]) 1)]) g)))
  (check-regexp-match #rx"fn without a delegate type needs an expected delegate type or annotated"
                      (tui-error '(let ([f (fn ([x : int] y) x)]) f)))
  (check-regexp-match #rx"expected a parameter name"
                      (tui-error '(let ([f (fn (1) 1)]) f))))

(test-case "a bare fn argument takes its delegate type from the chosen overload"
  (define (int-list . vals)
   `(let ([xs (new |System.Collections.Generic.List<int32>|)])
      ,@(for/list ([v vals]) `(.Add xs ,v))
      xs))
  (check-equal? (run-tui `(let ([xs ,(int-list 1 2)])
                           (.ForEach xs (fn (x) (Console.WriteLine x)))))
               '("1" "2"))
  (check-equal? (run-tui `(let ([xs ,(int-list 1 2 3)])
                           (Console.WriteLine (.Find xs (fn (x) (= (% x 2) 0))))))
               '("2"))
  (check-equal? (run-tui `(let ([xs ,(int-list 1 2)] [k 10])
                           (.ForEach xs (fn (x) (Console.WriteLine (+ x k))))))
               '("11" "12"))
  (check-equal? (run-tui `(let ([xs ,(int-list 5)])
                           (.ForEach xs (fn Action<int32> (x) (Console.WriteLine x)))))
               '("5"))
  (check-regexp-match #rx"no overload of .*ForEach accepts \\(fn/2\\)"
                     (tui-error `(let ([xs ,(int-list 1)]) (.ForEach xs (fn (x y) x)))))
  (check-regexp-match #rx"no overload of Console.WriteLine accepts \\(fn/1\\)"
                     (tui-error '(Console.WriteLine (fn (x) x))))
  (check-regexp-match #rx"parameter x is string but System.Action<int32> takes int32"
                     (tui-error `(let ([xs ,(int-list 1)])
                                   (.ForEach xs (fn ([x : string]) (Console.WriteLine x)))))))

(test-case "generic methods bind type arguments through base types"
  (define (int-list . vals)
   `(let ([xs (new |System.Collections.Generic.List<int32>|)])
      ,@(for/list ([v vals]) `(.Add xs ,v))
      xs))
  (check-equal? (run-tui '(using "System.Core") '(open System.Linq)
                         `(Console.WriteLine (Enumerable.Count ,(int-list 1 2 3))))
               '("3"))
  (check-equal? (run-tui '(using "System.Core") '(open System.Linq)
                         `(Console.WriteLine (Enumerable.Count<int32> ,(int-list 1 2))))
               '("2")))

(test-case "a bare fn argument fixes a method type parameter that only it mentions"
  (define (int-list . vals)
   `(let ([xs (new |System.Collections.Generic.List<int32>|)])
      ,@(for/list ([v vals]) `(.Add xs ,v))
      xs))
  (check-equal? (run-tui '(using "System.Core") '(open System.Linq)
                         `(Console.WriteLine
                           (Enumerable.Sum (Enumerable.Select ,(int-list 1 2 3) (fn (x) (* x 2))))))
               '("12"))
  (check-equal? (run-tui '(using "System.Core") '(open System.Linq)
                         `(Console.WriteLine
                           (Enumerable.Count (Enumerable.Where ,(int-list 1 2 3) (fn (x) (> x 2))))))
               '("1"))
  (check-equal? (run-tui '(using "System.Core") '(open System.Linq)
                         `(let ([k 10])
                            (Console.WriteLine
                             (Enumerable.Count (Enumerable.Select ,(int-list 1 2) (fn (x) (+ x k)))))))
               '("2")))

(test-case "an unannotated fn parameter takes the type of the one overload a call in its body fixes"
  (check-equal? (run-tui '(let ([f (fn (s) (String.IsNullOrEmpty s))])
                            (Console.WriteLine (.Invoke f ""))))
               '("True"))
  (check-equal? (run-tui '(let ([xs (new |System.Collections.Generic.List<int32>|)])
                            (let ([add (fn (x) (.Add xs x))])
                              (.Invoke add 4)
                              (.Invoke add 6)
                              (Console.WriteLine (.Contains xs 6)))))
               '("True"))
  (check-regexp-match #rx"fn without a delegate type needs"
                     (tui-error '(let ([f (fn (x) (Console.WriteLine x))]) f))))

(test-case "lambdas nest and share mutable captures through their generated classes"
  (check-equal? (run-tui '(define (outer [a : int]) : |Func<int32, int32>|
                            (let ([b 10])
                              (fn |Func<int32, int32>| (x)
                                (.Invoke (fn Func<int32> () (+ a (+ b x)))))))
                         '(Console.WriteLine (.Invoke (outer 1) 100)))
                '("111"))
  (check-equal? (run-tui '(let ([c 0])
                            (let ([bump (fn Action () (set! c (+ c 1)))])
                              (.Invoke bump)
                              (.Invoke bump)
                              (Console.WriteLine c))))
                '("2"))
  (check-equal? (run-tui '(let ([k 5])
                            (Console.WriteLine (.Invoke (fn Func<int32> () (* k 2)))))
                         '(let ([m 7])
                            (Console.WriteLine (.Invoke (fn Func<int32> () (+ m 1))))))
                '("10" "8")))

(test-case "fn captures a variable that a macro introduces"
  (check-equal?
   (run-tui '(define-syntax-rule (counter-from start)
               (let ([n start]) (fn Func<int32> () (set! n (+ n 1)) n)))
            '(let ([next (counter-from 10)])
               (Console.WriteLine (.Invoke next))
               (Console.WriteLine (.Invoke next))))
   '("11" "12")))

(test-case "each inference source works with and without annotations"
  (define (int-list . vals)
   `(let ([xs (new |System.Collections.Generic.List<int32>|)])
      ,@(for/list ([v vals]) `(.Add xs ,v))
      xs))
  ; expected type: a define return type, an annotated parameter checked against it
  (check-equal? (run-tui '(define (sq) : |Func<int32, int32>| (fn ([n : int]) (* n n)))
                         '(Console.WriteLine (.Invoke (sq) 9)))
                '("81"))
  (check-equal? (run-tui '(define (f) : |Func<string, bool>| (fn (s) (String.IsNullOrEmpty s)))
                         '(Console.WriteLine (.Invoke (f) "x")))
                '("False"))
  (check-equal? (run-tui '(define (hi) : |System.Action| (fn () (Console.WriteLine "hi")))
                         '(.Invoke (hi)))
                '("hi"))
  (check-equal? (run-tui '(define (apply2 [f : |Func<int32, int32>|] [x : int]) : int
                            (.Invoke f x))
                         '(Console.WriteLine (apply2 (fn ([k : int]) (+ k 1)) 4)))
                '("5"))
  (check-regexp-match #rx"parameter s is string but .*Func<int32, int32> takes int32"
                      (tui-error '(define (sq) : |Func<int32, int32>| (fn ([s : string]) s))))
  ; overload: an annotated parameter, an unannotated one fixed by the overload, and an
  ; unannotated one whose body call is overloaded (so the call gives nothing and the overload does)
  (check-equal? (run-tui `(let ([xs ,(int-list 1 2)])
                            (.ForEach xs (fn ([x : int]) (Console.WriteLine x)))))
                '("1" "2"))
  (check-equal? (run-tui '(let ([names (new |System.Collections.Generic.List<string>|)])
                            (.Add names "a") (.Add names "") (.Add names "b")
                            (Console.WriteLine (.RemoveAll names (fn (s) (String.IsNullOrEmpty s))))
                            (Console.WriteLine (.get_Count names))))
                '("1" "2"))
  (check-equal? (run-tui '(let ([names (new |System.Collections.Generic.List<string>|)])
                            (.Add names "a") (.Add names "b")
                            (.ForEach names (fn (s) (Console.WriteLine s)))))
                '("a" "b"))
  (check-equal? (run-tui '(let ([t (new System.Threading.Thread (fn () (Console.WriteLine "thread")))])
                            (.Start t)
                            (.Join t)))
                '("thread"))
  ; generic method: the bare fn fixes a type argument, with an annotation or from the receiver
  (check-equal? (run-tui '(using "System.Core") '(open System.Linq)
                         `(Console.WriteLine
                           (Enumerable.Sum (Enumerable.Select ,(int-list 1 2 3) (fn ([x : int]) (* x 2))))))
                '("12"))
  (check-equal? (run-tui '(using "System.Core") '(open System.Linq)
                         '(let ([names (new |System.Collections.Generic.List<string>|)])
                            (.Add names "a") (.Add names "") (.Add names "b")
                            (Console.WriteLine
                             (Enumerable.Count (Enumerable.Where names (fn (s) (String.IsNullOrEmpty s)))))))
                '("1"))
  ; body: partial annotation, where the unannotated parameter is fixed by a body call
  (check-equal? (run-tui '(let ([f (fn ([k : int] s) (String.IsNullOrEmpty s))])
                            (Console.WriteLine (.Invoke f 3 ""))
                            (Console.WriteLine (.Invoke f 3 "x"))))
                '("True" "False"))
  (check-regexp-match #rx"fn without a delegate type needs"
                      (tui-error '(let ([f (fn ([k : int] s) k)]) f)))
  ; zero parameters: the type comes from the overload, the expected type, or the body
  (check-equal? (run-tui '(let ([fs (new |System.Collections.Generic.List<Func<int32>>|)])
                            (.Add fs (fn () 5))
                            (Console.WriteLine (.Invoke (.get_Item fs 0)))))
                '("5"))
  (check-equal? (run-tui '(define (run [f : |Func<int32>|]) : int (.Invoke f))
                         '(Console.WriteLine (run (fn () 7))))
                '("7")))

(test-case "the REPL lifts lambdas defined in an interaction"
  (define mod (build-path examples "basics.rkt"))
  (dynamic-require mod #f)
  (define ns (module->namespace mod))
  (define (repl form)
    (lines (with-output-to-string
             (λ () (parameterize ([current-namespace ns])
                     (eval (namespace-syntax-introduce
                            (datum->syntax #f (cons '#%top-interaction form)))))))))
  (check-equal? (repl '(define (mk) : |Func<int32, int32>| (fn |Func<int32, int32>| (n) (+ n 1))))
                '())
  (check-equal? (repl '(Console.WriteLine (.Invoke (mk) 1))) '("2"))
  (check-equal? (repl '(let ([k 4]) (Console.WriteLine (.Invoke (fn Func<int32> () (* k k))))))
                '("16")))

(test-case "LINQ works with stored lambdas, library macros and reference conversions"
  (check-equal? (run-example "linq.rkt") '("56" "2275" "50" "3" "6")))

(test-case "a List<string> argument converts to IEnumerable<string>, not object"
  (check-equal?
   (run-tui '(let ([names (new |System.Collections.Generic.List<string>|)])
               (.Add names "a") (.Add names "b")
               (Console.WriteLine (String.Concat names))))
   '("ab")))

(test-case "phase 1 macros read overloads, supertypes and members from metadata"
  (check-equal? (run-example "phase1.rkt")
                '("11" "9" "True" "False" "True" "False" "4" "Equals" "55")))

(test-case "compose checks that both arguments are Func<int32, int32>"
  (define fnlib-path (path->string (build-path examples "fnlib.rkt")))
  (check-regexp-match #rx"compose needs Func<int32, int32> arguments, got int32"
                      (tui-error `(require (file ,fnlib-path)) '(compose 1 square))))

(test-case "module values are static fields set in source order, and set! updates them"
  (check-equal? (run-example "values.rkt") '("5" "10" "17"))
  (check-equal? (run-tui '(define (get) : int b)
                         '(define b : int 7)
                         '(Console.WriteLine (get)))
                '("7")))

(test-case "a module value cannot be read before its definition"
  (check-regexp-match #rx"module value used before it is defined"
                      (tui-error '(define a : int (+ b 1)) '(define b : int 1)))
  (check-regexp-match #rx"module value used before it is defined"
                      (tui-error '(define a : int a))))

(test-case "the REPL does not define module values"
  (define mod (build-path examples "basics.rkt"))
  (dynamic-require mod #f)
  (define ns (module->namespace mod))
  (check-exn #rx"module values can only be defined in a module"
             (λ () (parameterize ([current-namespace ns])
                     (eval (namespace-syntax-introduce
                            (datum->syntax #f '(#%top-interaction define x : int 1)))))))
  (check-regexp-match #rx"expected \\(define name : type expr\\)"
                      (tui-error '(define x : int))))

