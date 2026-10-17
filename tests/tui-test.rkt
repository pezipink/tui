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
