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
