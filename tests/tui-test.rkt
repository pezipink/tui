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

; Expands a module read from src, and returns the (text type) of each 'mouse-over-tooltips leaf in
; the expansion, where text is the source text the leaf covers.
(define (expanded-tooltips src)
  (parameterize ([current-namespace (make-base-namespace)])
    (let find ([s (expand (read-syntax 'm (open-input-string src)))])
      (cond
        [(syntax? s)
         (define leaves (syntax-property s 'mouse-over-tooltips))
         (append (for/list ([leaf (in-list (or leaves '()))])
                   (list (substring src (vector-ref leaf 1) (vector-ref leaf 2))
                         (vector-ref leaf 3)))
                 (find (syntax-e s)))]
        [(pair? s) (append (find (car s)) (find (cdr s)))]
        [else '()]))))

(define hover-src
  "(module m tui (define (fact [n : int]) : int (if (< n 2) 1 (* n (fact (- n 1)))))
  (let ([x (fact 5)]) (Console.WriteLine (+ x 1))))")

(test-case "hover tooltips type arrays, aref and static field reads"
  (define leaves
    (expanded-tooltips
     "(module m tui
  (define (first [xs : |int32[]|]) : int (aref xs 0))
  (define (max-value) : int System.Int32.MaxValue)
  (let ([a (array int32 1 2)])
    (set! (aref a 0) 3)
    (Console.WriteLine (first a))))"))
  (check-not-false (member '("xs" "int32[]") leaves))
  (check-not-false (member '("a" "int32[]") leaves))
  (check-not-false (member '("System.Int32.MaxValue" "int32") leaves)))

(test-case "hover tooltips mark each typed expression with its type"
  ; A compound expression is marked on its parens; an atom on its own text.
  (check-equal? (expanded-tooltips hover-src)
                '(("n" "int32")
                  ("(" "int32") (")" "int32")
                  ("(" "bool") (")" "bool")
                  ("n" "int32") ("2" "int32") ("1" "int32")
                  ("(" "int32") (")" "int32")
                  ("n" "int32")
                  ("(" "int32") (")" "int32")
                  ("(" "int32") (")" "int32")
                  ("n" "int32") ("1" "int32")
                  ("(" "void") (")" "void")
                  ("x" "int32")
                  ("(" "int32") (")" "int32") ("5" "int32")
                  ("(" "void") (")" "void")
                  ("x" "int32") ("1" "int32"))))

(test-case "hover tooltips cover let and set! binders, fn parameters and call arguments"
  (define leaves
    (expanded-tooltips
     "(module m tui
  (using \"System.Core\") (open System.Linq)
  (define counter : int 0)
  (let ([n 0])
    (set! n 5)
    (set! counter 7)
    (Console.WriteLine (.get_Item1 (System.Tuple.Create 1 \"a\"))))
  (let ([p (System.Tuple.Create 1 \"a\")])
    (Console.WriteLine (.get_Item1 p))
    (let ([sq (fn ([x : int]) (* x x))])
      (Console.WriteLine (.Invoke sq 3))))
  (let ([t (System.Tuple.Create 1 \"a\")]) (Console.WriteLine t)))"))
  (for ([leaf (in-list '(("n" "int32")
                         ("counter" "int32")
                         ("p" "System.Tuple\\<int32, string\\>")
                         ("sq" "System.Func\\<int32, int32\\>")
                         ("x" "int32")
                         ("t" "System.Tuple\\<int32, string\\>")))])
    (check-not-false (member leaf leaves) (format "missing leaf ~s" leaf))))

(test-case "hover tooltips are logged on online-check-syntax for racket-langserver"
  (define rcv (make-log-receiver (current-logger) 'info 'online-check-syntax))
  (expanded-tooltips hover-src)
  (define v (sync/timeout 5 rcv))
  (check-true (vector? v))
  (check-equal? (vector-ref v 3) 'online-check-syntax)
  (define carriers (vector-ref v 2))
  (check-equal? (length carriers) 1)
  (check-equal? (length (syntax-property (car carriers) 'mouse-over-tooltips)) 27))

(test-case "scopes record each binder with its type and the span where it is visible"
  (define src
    "(module m tui
  (define counter : int 0)
  (define (f [n : int]) : int (let ([n (+ n 1)]) n))
  (let ([x 1]) (set! x 2) (Console.WriteLine x)))")
  (define rcv (make-log-receiver (current-logger) 'info 'tui-scopes))
  (parameterize ([current-namespace (make-base-namespace)])
    (expand (read-syntax 'm (open-input-string src))))
  (define recs (cadr (vector-ref (sync/timeout 5 rcv) 2)))
  (define (pos rx) (add1 (car (cadr (regexp-match-positions rx src)))))
  (define (at rx) (add1 (caar (regexp-match-positions rx src))))
  (define let-form "(let ([n (+ n 1)]) n)")
  (define let-start (at #rx"\\(let \\(\\[n"))
  (define body-n-start (+ let-start (string-length "(let ([n (+ n 1)]) ")))
  (define x-body "(set! x 2) (Console.WriteLine x)")
  (define x-start (at #rx"\\(set! x"))
  (check-not-false (member (vector "counter" "int32" (pos #px"\\(define (counter) :") 7 0 +inf.0)
                           recs))
  (check-not-false (member (vector "f" "(int32) -> int32" (pos #px"\\(define \\((f) \\[") 1 0 +inf.0)
                           recs))
  (check-not-false (member (vector "n" "int32" (pos #px"\\[(n) :") 1
                                   let-start (+ let-start (string-length let-form)))
                           recs))
  (check-not-false (member (vector "n" "int32" (pos #px"\\(\\[(n) \\(\\+") 1
                                   body-n-start (add1 body-n-start))
                           recs))
  (check-not-false (member (vector "x" "int32" (pos #px"\\(\\[(x) 1\\]") 1
                                   x-start (+ x-start (string-length x-body)))
                           recs))
  ; set! assigns an existing binder, so it records no binder of its own.
  (check-equal? (length (filter (λ (r) (equal? (vector-ref r 0) "x")) recs)) 1))

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
  (check-equal?
   (run-tui '(let ([t (System.Tuple.Create<int> 1 2)])
               (Console.WriteLine (.get_Item2 t))))
   '("2"))
  (check-regexp-match #rx"no overload"
                      (tui-error '(|System.Tuple.Create<int, int, int>| 1 2))))

(test-case "generic parameters are inferred from list and delegate variable arguments"
  (check-equal?
   (run-tui '(using "System.Core") '(open System.Linq)
            '(let ([xs (new |System.Collections.Generic.List<int32>|)])
               (.Add xs 1) (.Add xs 2) (.Add xs 3)
               (let ([even? (fn ([x : int]) (= (% x 2) 0))])
                 (Console.WriteLine (Enumerable.Count (Enumerable.Where xs even?)))
                 (Console.WriteLine (Enumerable.Sum (Enumerable.Select xs (fn ([x : int]) (* x x))))))))
   '("1" "14")))

(test-case "type arguments may be _ or omitted at the end, and the rest are inferred"
  (define (linq call)
    (run-tui '(using "System.Core") '(open System.Linq)
             `(let ([xs (new |System.Collections.Generic.List<int32>|)])
                (.Add xs 1) (.Add xs 2) (.Add xs 3) (.Add xs 4)
                (let ([even? (fn ([x : int]) (= (% x 2) 0))])
                  (Console.WriteLine ,call)))))
  (define square-fn '(fn ([x : int]) (* x x)))
  (check-equal? (linq '(Enumerable.Count<_> (Enumerable.Where<_> xs even?))) '("2"))
  (check-equal? (linq '(Enumerable.Count<int32> (Enumerable.Where xs even?))) '("2"))
  (check-equal? (linq `(let ([evens (Enumerable.Where xs even?)])
                         (Enumerable.Sum (Enumerable.Select<_> evens ,square-fn))))
                '("20"))
  (check-equal? (linq `(let ([evens (Enumerable.Where xs even?)])
                         (Enumerable.Sum (|Enumerable.Select<int32, _>| evens ,square-fn))))
                '("20"))
  (check-equal? (linq `(let ([evens (Enumerable.Where xs even?)])
                         (Enumerable.Sum (|Enumerable.Select<_, _>| evens ,square-fn))))
                '("20"))
  (define (linq-error call)
    (tui-error '(using "System.Core") '(open System.Linq)
               `(Console.WriteLine ,call)))
  (check-regexp-match #rx"no overload of Enumerable.Count accepts \\(System.Collections.Generic.List<int32>\\) with type arguments <int32, int32>"
                      (linq-error '(|Enumerable.Count<int32, int32>|
                                    (new |System.Collections.Generic.List<int32>|))))
  (check-regexp-match #rx"with type arguments <_>"
                      (linq-error '(Enumerable.Count (Enumerable.Empty<_>))))
  (check-regexp-match #rx"with type arguments <string>"
                      (linq-error '(Enumerable.Count<string>
                                    (new |System.Collections.Generic.List<int32>|)))))

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
                '("0" "False" "0" "0" "0" "System.Collections.Generic.List<int32>" "True" "False" "System.Int32" "System.Text.StringBuilder" "Point")))

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

(test-case "typeof gives the System.Type of a primitive, class or local record"
  (check-equal? (run-tui '(Console.WriteLine (.get_FullName (typeof int64)))
                        '(Console.WriteLine (.get_FullName (typeof System.Text.StringBuilder))))
               '("System.Int64" "System.Text.StringBuilder")))

(test-case "typeof rejects void and types with type arguments"
  (check-regexp-match #rx"typeof needs a type, got void" (tui-error '(typeof void)))
  (check-regexp-match #rx"typeof supports primitives and classes without type arguments, got System.Collections.Generic.List<int32>"
                     (tui-error '(typeof System.Collections.Generic.List<int32>))))

(test-case "sum-of reads the element type from the closed List's metadata"
  (check-regexp-match #rx"sum-of needs a list of int32 or string elements, got bool"
                      (tui-error `(require (file ,(path->string (build-path examples "lists.rkt"))))
                                 '(sum-of (list-of #t))))
  (check-regexp-match #rx"sum-of needs a list .* got a non-list"
                      (tui-error `(require (file ,(path->string (build-path examples "lists.rkt"))))
                                 '(sum-of 1))))

(test-case "typedList no longer takes a dictionary's value type as the element type"
  (check-regexp-match #rx"sum-of needs a list .* got a non-list"
                      (tui-error `(require (file ,(path->string (build-path examples "lists.rkt"))))
                                 '(sum-of (new |System.Collections.Generic.Dictionary<string,int32>|)))))

(test-case "the type-patterns example exercises generic, member, named and combinator patterns"
  (check-equal? (run-example "type-patterns.rkt")
                '("2" "7" "string => int32" "counted" "uncounted" "string" "int32" "True"
                  "numeric" "scalar" "scalar" "exactly int32" "not int32" "something else"
                  "a list of lists of int32" "a scalar" "a sequence of scalars" "a scalar"
                  "0" "0" "False" "dict string => list of int32" "unsupported")))

(test-case "collect fills lists, sets, dictionaries and nested collections, and rejects non-collections"
  (check-equal? (run-example "collect.rkt") '("3" "2" "2" "2" "2" "3"))
  (check-regexp-match #rx"System.Text.StringBuilder is not a collection type"
                      (tui-error `(require (file ,(path->string (build-path examples "collect.rkt"))))
                                 '(collect |System.Text.StringBuilder| 1))))

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
  (check-equal? (repl '(define (cube [x : int]) (* x (* x x)))) '())
  (check-equal? (repl '(Console.WriteLine (cube 3))) '("27"))
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

(test-case "a numeric literal can be the receiver of a member call"
  (check-equal?
   (run-tui '(Console.WriteLine (1.ToString))
            '(Console.WriteLine (1.5.ToString))
            '(Console.WriteLine (5000000000.ToString)))
   '("1" "1.5" "5000000000")))

(test-case "a member missing on a numeric literal is a compile error"
  (check-not-false (tui-error '(Console.WriteLine (1.NoSuchMember)))))

(test-case "array-typed and struct-typed members are visible and callable"
  (check-equal?
   (run-tui '(Console.WriteLine (.get_Length (.ToCharArray "abc")))
            '(Console.WriteLine (.Length (.Split "a,b,c" (.ToCharArray ","))))
            '(Console.WriteLine (String.Join "-" (.Split "a b c" (.ToCharArray " "))))
            '(Console.WriteLine (> (.Length (System.IO.Path.GetInvalidFileNameChars)) 0))
            '(Console.WriteLine (.ToString (.DayOfWeek (DateTime.Parse "2024-01-01"))))
            '(Console.WriteLine (Int32.Parse "42")))
   '("3" "3" "a-b-c" "True" "Monday" "42")))

(test-case "null is typed from the expected type and works as an argument"
  (check-equal?
   (run-tui '(define (empty? [s : string]) : bool (null? s))
            '(Console.WriteLine (empty? null))
            '(Console.WriteLine (null? "x"))
            '(Console.WriteLine (String.IsNullOrEmpty null))
            '(Console.WriteLine (= "x" null))
            '(Console.WriteLine (= null (System.Activator.CreateInstance<System.Text.StringBuilder>)))
            '(let ([xs (list-of "a")])
               (.Add xs null)
               (Console.WriteLine (.get_Count xs))))
   '("True" "False" "True" "False" "False" "2")))

(test-case "null reaches constructors and generic methods"
  (check-equal?
   (run-tui '(using "System.Core") '(open System.Linq)
            '(Console.WriteLine (.get_Length (new System.Text.StringBuilder null)))
            '(Console.WriteLine (Enumerable.Contains (list-of "a") null)))
   '("0" "False")))

(test-case "null-of declares a typed null where there is no expected type"
  (check-equal?
   (run-tui '(let ([s (null-of string)])
               (Console.WriteLine (null? s))
               (set! s "x")
               (Console.WriteLine (null? s)))
            '(let ([sb (null-of System.Text.StringBuilder)])
               (Console.WriteLine (null? sb))))
   '("True" "False" "True"))
  (check-regexp-match #rx"null-of needs a reference type, got int32" (tui-error '(null-of int32)))
  (check-regexp-match #rx"unknown type" (tui-error '(null-of NoSuchType))))

(test-case "null is rejected where it has no reference type"
  (check-regexp-match #rx"ambiguous call to Console.WriteLine with \\(null\\)"
                      (tui-error '(Console.WriteLine null)))
  (check-regexp-match #rx"null needs an expected reference type" (tui-error '(let ([x null]) x)))
  (check-regexp-match #rx"null cannot have type int32" (tui-error '(define (f) : int null)))
  (check-regexp-match #rx"null cannot have type int32"
                      (tui-error '(define (k [n : int]) : int n) '(k null)))
  (check-regexp-match #rx"cannot compare int32 with null" (tui-error '(= 1 null)))
  (check-regexp-match #rx"cannot compare null with null" (tui-error '(= null null)))
  (check-regexp-match #rx"null\\? needs a value, not a bare null" (tui-error '(null? null)))
  (check-regexp-match #rx"null\\? needs a reference type, got int32" (tui-error '(null? 1))))

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

; Writes src to a file in a fresh temp directory, and returns the path.
(define (write-module src-text)
  (define dir (make-temporary-directory "tui-src-~a"))
  (define src (build-path dir "prog.rkt"))
  (display-to-file src-text src)
  (values dir src))

; The (line column end-line end-column file) of each .line directive in a program's instructions.
(define (line-directives p)
  (let find ([d (list (tui-program-methods p) (tui-program-main p))])
    (cond
      [(and (pair? d) (eq? (car d) '.line))
       (list (list (list-ref d 1) (list-ref d 2) (list-ref d 3) (list-ref d 4) (list-ref d 5)))]
      [(pair? d) (append (find (car d)) (find (cdr d)))]
      [else '()])))

(define line-program-text
  "#lang tui
(define (add [a : int] [b : int]) : int
  (+ a
     b))
(define (twice [x : int]) : int
  (let ([y (add x x)])
    (Console.WriteLine y)
    y))
(Console.WriteLine (twice 4))
")

(test-case "statements carry .line directives with their source spans, with CRLF or LF line ends"
  (for ([crlf? (in-list '(#t #f))])
    (define-values (dir src)
      (write-module (if crlf?
                        (string-replace line-program-text "\n" "\r\n")
                        line-program-text)))
    (define p (dynamic-require src 'tui-program))
    (define file (path->string src))
    (check-equal? (sort (line-directives p) < #:key car)
                  `((3 2 4 7 ,file) (6 2 8 6 ,file) (7 4 7 25 ,file)
                    (8 4 8 5 ,file) (9 0 9 29 ,file)))
    (delete-directory/files dir)))

(test-case "statements from a macro template get no .line directive"
  (define-values (dir src)
    (write-module "#lang tui
(define-syntax-rule (show x) (Console.WriteLine x))
(show 4)
(Console.WriteLine 5)
"))
  (define p (dynamic-require src 'tui-program))
  (define ds (line-directives p))
  ; the template is on line 2, and no directive may point at it
  (check-false (for/or ([d (in-list ds)]) (= (car d) 2)))
  ; a plain statement next to it still gets one
  (check-true (for/or ([d (in-list ds)]) (equal? (car d) 4)))
  (delete-directory/files dir))

(test-case "tui-write-exe writes a PDB beside the exe"
  (define p (dynamic-require (build-path examples "basics.rkt") 'tui-program))
  (define dir (make-temporary-directory "tui-pdb-~a"))
  (define exe (build-path dir "pdb.exe"))
  (tui-write-exe p exe)
  (define pdb (path-replace-extension exe #".pdb"))
  (check-true (file-exists? pdb))
  (check-true (> (file-size pdb) 0))
  (delete-directory/files dir))

(test-case "tui-write-exe builds an x64 PE32+ image with a debuggable attribute"
  (define p (dynamic-require (build-path examples "basics.rkt") 'tui-program))
  (define dir (make-temporary-directory "tui-x64-~a"))
  (define exe (build-path dir "x64.exe"))
  (tui-write-exe p exe)
  (define bs (file->bytes exe))
  (define pe (integer-bytes->integer bs #f #f #x3C #x40))
  (check-equal? (integer-bytes->integer bs #f #f (+ pe 4) (+ pe 6)) #x8664 "machine is AMD64")
  (check-equal? (integer-bytes->integer bs #f #f (+ pe 24) (+ pe 26)) #x20B "optional header is PE32+")
  (check-true (regexp-match? #rx#"DebuggableAttribute" bs) "DebuggableAttribute is referenced")
  (delete-directory/files dir))

; The metadata strings are NUL-terminated, so a name matches as \0name\0.
(test-case "parameters, locals and catch variables keep their names, and shadowed names get a suffix"
  (define-values (dir src)
    (write-module "#lang tui
(define (f [n : int]) : int
  (let ([acc 1])
    (let ([acc (+ acc n)])
      (try acc
        (catch [e : System.Exception] 0)))))
(Console.WriteLine (f 2))
"))
  (define p (dynamic-require src 'tui-program))
  (define exe (build-path dir "names.exe"))
  (tui-write-exe p exe)
  (define pdb (file->bytes (path-replace-extension exe #".pdb")))
  (define bs (file->bytes exe))
  (check-true (regexp-match? #rx#"\0n\0" bs) "parameter n is named in the exe")
  (check-true (regexp-match? #rx#"\0acc\0" pdb) "local acc is named in the PDB")
  (check-true (regexp-match? #rx#"\0acc_1\0" pdb) "shadowed acc is suffixed")
  (check-true (regexp-match? #rx#"\0e\0" pdb) "catch variable e is named in the PDB")
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

(test-case "a delegate value is called as (f x), as well as with .Invoke"
  (check-equal? (run-tui '(let ([sq (fn ([n : int]) (* n n))])
                            (Console.WriteLine (sq 9))
                            (Console.WriteLine (.Invoke sq 3))))
                '("81" "9"))
  (check-equal? (run-tui '(define (sq) : |Func<int32, int32>| (fn (n) (* n n)))
                         '(define (apply-fn [f : |Func<int32, int32>|] [x : int]) : int (f x))
                         '(Console.WriteLine (apply-fn (sq) 4)))
                '("16"))
  (check-equal? (run-tui '(let ([five (fn () 5)])
                            (Console.WriteLine (five))))
                '("5"))
  (check-equal? (run-tui '(let ([k 3])
                            (let ([add (fn ([a : int]) (+ a k))])
                              (Console.WriteLine (add 4))
                              (set! k 10)
                              (Console.WriteLine (add 4)))))
                '("7" "14"))
  (check-equal? (run-tui '(let ([sq (fn ([n : int]) (* n n))])
                            (let ([f (fn (x) (sq x))])
                              (Console.WriteLine (.Invoke f 6)))))
                '("36"))
  (check-equal? (run-tui '(define g : |Func<int32, int32>| (fn (n) (* n 2)))
                         '(Console.WriteLine (g 5)))
                '("10"))
  (check-regexp-match #rx"n is int32, not a delegate, so it cannot be called"
                      (tui-error '(let ([n 1]) (n 2)))))

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
                '("18" "19" "True" "False" "True" "False" "4" "Equals" "55")))

(test-case "the XML example reads and builds documents with System.Xml.Linq"
  (check-equal? (run-example "xml.rkt")
                '("3" "Dune" "28.75" "Dune" "Neuromancer"
                  "<shelf>" "  <book id=\"9\">Hyperion</book>" "  <book id=\"10\">Ubik</book>" "</shelf>"
                  "title" "lang" "element" "attribute")))

(test-case "the JSON example parses, walks and re-serialises JSON text"
  (check-equal? (run-example "json.rkt")
                '("Corner \"Books\"" "7.25" "Abc"
                  "{\"store\":{\"name\":\"Corner \\\"Books\\\"\",\"open\":true,\"books\":[{\"title\":\"Dune\",\"price\":9.5},{\"title\":\"Neuromancer\",\"price\":7.25}],\"owner\":null},\"tags\":[\"sf\",\"Abc\"]}"
                  "{\"title\":\"Dune\",\"year\":1965,\"rating\":4.5,\"tags\":[\"sf\",\"classic\"],\"loaned\":false}"
                  "sf")))

(test-case "threading's ~> and ~>> compile to ordinary Tui calls"
  (check-equal? (run-example "threading.rkt") '("14" "5" "11" "TUI" "pipe" "3"))
  (check-regexp-match #rx"unbound function"
                     (tui-error '(require threading) '(Console.WriteLine (as~> 3 x (+ x 1))))))

(test-case "a.b on a local calls or reads a member of that local, even when a type has the same name"
  (check-equal? (run-tui '(define (h [s : string]) : int s.Length)
                         '(define (k [Console : string]) : int Console.Length)
                         '(define (g [s : string]) : int (.Length s))
                         '(define (sub [s : string]) : string (s.Substring 1))
                         '(Console.WriteLine (h "four"))
                         '(Console.WriteLine (k "abcd"))
                         '(Console.WriteLine (g "hello"))
                         '(Console.WriteLine (sub "abc")))
                '("4" "4" "5" "bc"))
  (check-equal? (run-tui '(Console.WriteLine (null? Console.Out))) '("False")))

(test-case "threaded member chains read properties, call methods and fall back to getters"
  (check-equal? (run-tui '(require threading)
                         '(define (h [s : string]) : int s.Length)
                         '(Console.WriteLine (~> "tui" .Length))
                         '(Console.WriteLine (~> 5 .ToString .Length))
                         '(Console.WriteLine (~> "  tui  " (.Trim) .Length))
                         '(Console.WriteLine (h (~> "ab" (.PadRight 5))))
                         '(let ([n (~> "four" .Length)]) (Console.WriteLine n)))
                '("3" "1" "3" "5" "4")))

(test-case "a member that does not exist is named in the error"
  (check-regexp-match #rx"has no public instance method or property Nope"
                      (tui-error '(define (f [s : string]) : int s.Nope)))
  (check-regexp-match #rx"has no public instance method or property Nope"
                      (tui-error '(define (f [s : string]) : int (.Nope s))))
  (check-regexp-match #rx"has no public instance method Nope"
                      (tui-error '(define (f [s : string]) : int (s.Nope 1))))
  (check-regexp-match #rx"Console has no public static method or property Nope"
                      (tui-error '(define (f) : int Console.Nope))))

(test-case "hover marks dotted members and the steps of a threaded chain"
  (define tips
    (expanded-tooltips
     "(module m tui (require threading) (define (h [s : string]) : int s.Length)
  (define (k) : int (~> Console.Out .ToString .Length)))"))
  (check-not-false (member '("s.Length" "int32") tips))
  (check-not-false (member '("s" "string") tips))
  (check-not-false (member '("Console.Out" "System.IO.TextWriter") tips))
  ; A threaded step is a compound expression, so it is marked on its first and last characters.
  (check-not-false (member '("." "string") tips)))

(test-case "phase 1 definitions: syntax classes, simple macros and phase-1 helpers"
  (check-equal? (run-example "syntax-classes.rkt") '("42" "10" "29"))
  (check-equal? (run-tui '(require (for-syntax racket/base syntax/parse))
                        '(define-syntaxes (inc) (lambda (stx) (syntax-case stx () [(_ x) #'(+ x 1)])))
                        '(Console.WriteLine (inc 41)))
                '("42"))
  (check-equal? (run-tui '(require (for-syntax racket/base syntax/parse))
                        '(begin-for-syntax
                           (define-splicing-syntax-class pair
                             (pattern (~seq a:nat b:nat) #:attr total (+ (syntax-e #'a) (syntax-e #'b)))))
                        '(define-syntax-parser pair-total [(_ p:pair) #`#,(attribute p.total)])
                        '(Console.WriteLine (pair-total 3 4)))
                '("7"))
  (check-regexp-match #rx"twice: expected"
                     (tui-error '(require (for-syntax racket/base syntax/parse))
                                '(begin-for-syntax (define-syntax-class small (pattern n:nat)))
                                '(define-simple-macro (twice x:small) (+ x x))
                                '(Console.WriteLine (twice -1)))))

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

(test-case "return types and module values are inferred from the last expression"
  (check-equal? (run-example "inference.rkt") '("25" "50" "42"))
  (check-equal? (run-tui '(define-syntax-rule (twice e) (* 2 e))
                         '(define (f [x : int]) (twice x))
                         '(Console.WriteLine (f 21)))
                '("42"))
  (check-equal? (run-tui '(define (f [x : int]) (* x 2))
                         '(Console.WriteLine (f 21)))
                '("42"))
  (check-equal? (run-tui '(define (a [x : int]) (b x))
                         '(define (b [x : int]) (+ x 1))
                         '(Console.WriteLine (a 1)))
                '("2"))
  (check-equal? (run-tui '(define (show [x : int]) (Console.WriteLine x))
                         '(show 3))
                '("3"))
  (check-equal? (run-tui '(define a (+ 1 2))
                         '(define b (* a 2))
                         '(define (g [x : int]) (+ x b))
                         '(Console.WriteLine (g 1)))
                '("7")))

(test-case "an inferred return type that depends on itself is an error"
  (check-regexp-match #rx"cannot infer the return type of f, which depends on itself"
                      (tui-error '(define (f [n : int]) (if (< n 1) 1 (f (- n 1))))))
  (check-regexp-match #rx"cannot infer the return type of a, which depends on itself"
                      (tui-error '(define (a [x : int]) (b x))
                                 '(define (b [x : int]) (a x))))
  (check-equal? (run-tui '(define (f [n : int]) : int (if (< n 1) 1 (f (- n 1))))
                         '(Console.WriteLine (f 4)))
                '("1")))

(test-case "the exceptions example runs"
  (check-equal? (run-example "exceptions.rkt")
                '("2" "checked" "b is zero" "checked" "42" "not a number" "-1"
                  "5" "logging" "outer caught")))

(test-case "try catches typed and catch-all handlers and yields the value of the body or a handler"
  (check-equal? (run-tui '(define (safe [n : int]) : int
                            (try (if (< n 0) (throw (new System.ArgumentException "neg")) n)
                                 (catch [e : System.ArgumentException] 0)))
                         '(Console.WriteLine (safe 3))
                         '(Console.WriteLine (safe -1)))
                '("3" "0"))
  (check-equal? (run-tui '(define (f [n : int]) : int
                            (try (if (< n 0) (throw (new System.Exception "x")) n)
                                 (catch e 7)))
                         '(Console.WriteLine (f -1)))
                '("7"))
  (check-equal? (run-tui '(define (g [n : int]) : int
                            (let ([x (try (if (< n 0) (throw (new System.Exception "x")) n)
                                          (catch [e : System.Exception] 5))])
                              (+ x 1)))
                         '(Console.WriteLine (g -1))
                         '(Console.WriteLine (g 1)))
                '("6" "2")))

(test-case "finally runs on the normal path and when the exception leaves the try"
  (define defs
    '((define (f [n : int]) : void
        (try (if (< n 0) (throw (new System.Exception "x")) (Console.WriteLine "body"))
             (finally (Console.WriteLine "finally"))))))
  (check-equal? (apply run-tui (append defs '((f 1))))
                '("body" "finally"))
  (check-equal? (apply run-tui (append defs '((try (f -1) (catch e (Console.WriteLine "outer"))))))
                '("finally" "outer")))

(test-case "rethrow passes the exception of a catch handler to an outer try"
  (check-equal? (run-tui '(try
                            (try (throw (new System.InvalidOperationException "inner"))
                                 (catch [e : System.InvalidOperationException]
                                   (Console.WriteLine "first")
                                   (rethrow)))
                            (catch [e : System.Exception] (Console.WriteLine "outer"))))
                '("first" "outer")))

(test-case "handler variables are cells when a lambda reads them"
  (check-equal? (run-tui '(try (throw (new System.Exception "boom"))
                               (catch [e : System.Exception]
                                 (Console.WriteLine (.Invoke (fn () (.get_Message e)))))))
                '("boom")))

(test-case "a try in a let body inside a function, and a lambda in a try body, run"
  (check-equal? (run-tui '(define (h [n : int]) : void
                            (let ([x 5])
                              (try (if (< n 0) (throw (new System.Exception "x")) (Console.WriteLine x))
                                   (catch [e : System.Exception] (Console.WriteLine "caught")))))
                         '(h 1)
                         '(h -1)
                         '(try (Console.WriteLine (.Invoke (fn () 9)))
                               (catch e (Console.WriteLine "top"))))
                '("5" "caught" "9")))

(test-case "try, throw and rethrow are checked at expansion time"
  (check-regexp-match #rx"try must be a statement"
                      (tui-error '(Console.WriteLine (try 1 (catch [e : System.Exception] 2)))))
  (check-regexp-match #rx"throw needs an exception, got int32" (tui-error '(throw 1)))
  (check-regexp-match #rx"rethrow is only allowed in a catch handler" (tui-error '(rethrow)))
  (check-regexp-match #rx"rethrow is only allowed in a catch handler"
                      (tui-error '(try 1 (finally (rethrow)))))
  (check-regexp-match #rx"try needs a body" (tui-error '(try (catch [e : System.Exception] 1))))
  (check-regexp-match #rx"try needs a catch or finally clause" (tui-error '(try 1)))
  (check-regexp-match #rx"a catch-all clause must be the last catch clause"
                      (tui-error '(try 1 (catch e 2) (catch [x : System.Exception] 3))))
  (check-regexp-match #rx"finally must be the last clause of try"
                      (tui-error '(try 1 (finally (Console.WriteLine "f")) (catch [x : System.Exception] 3))))
  (check-regexp-match #rx"catch needs an exception type, got int32"
                      (tui-error '(try 1 (catch [x : int] 2))))
  (check-regexp-match #rx"catch handler has type string, but the try has type int32"
                      (tui-error '(define (f) : int (try 1 (catch [x : System.Exception] "s")))))
  (check-regexp-match #rx"a finally body must be void, got int32"
                      (tui-error '(try 1 (catch [x : System.Exception] 2) (finally 3))))
  (check-regexp-match #rx"cannot bind a value that never completes"
                      (tui-error '(let ([x (throw (new System.Exception "x"))]) x))))

(test-case "an inferred module value cannot use a later value or be void"
  (check-regexp-match #rx"module value used before it is defined"
                      (tui-error '(define a (+ b 1)) '(define b : int 1)))
  (check-regexp-match #rx"a module value cannot be void"
                      (tui-error '(define a (Console.WriteLine 1)))))

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
  (check-regexp-match #rx"expected \\(define name \\[: type\\] expr\\)"
                      (tui-error '(define x : int)))
  (check-exn #rx"record is only allowed at module level"
             (λ () (parameterize ([current-namespace ns])
                     (eval (namespace-syntax-introduce
                            (datum->syntax #f '(#%top-interaction record P [x : int32]))))))))

(test-case "a record has a constructor, get_ and set_ properties, and can be passed and returned"
  (check-equal? (run-example "records.rkt")
                '("3" "10" "7" "15" "7" "Ada" "36" "6"
                  "a Point" "a Line" "something else" "point" "segment" "other" "3" "int32" "7")))

(test-case "record declarations are checked"
  (check-regexp-match #rx"record is only allowed at module level"
                      (tui-error '(define (f) : int (let ([r (record P [x : int32])]) 1))))
  (check-regexp-match #rx"duplicate field name"
                      (tui-error '(record P [x : int32] [x : int32])))
  (check-regexp-match #rx"a record field cannot be void"
                      (tui-error '(record P [x : void])))
  (check-regexp-match #rx"P is already declared"
                      (tui-error '(record P [x : int32]) '(record P [y : int32])))
  (check-regexp-match #rx"unknown type"
                      (tui-error '(record Q [a : Later]) '(record Later [x : int32]))))

(test-case "record members are checked where they are used"
  (check-regexp-match #rx"has no public instance method or property z"
                      (tui-error '(record P [x : int32]) '(define (g [p : P]) : int p.z)))
  (check-regexp-match #rx"no overload of P.set_x accepts \\(string\\)"
                      (tui-error '(record P [x : int32])
                                 '(define (k [p : P]) : int (begin (set! p.x "s") 1))))
  (check-regexp-match #rx"no overload of new P accepts \\(\\)"
                      (tui-error '(record P [x : int32]) '(define (m) : P (new P)))))

(test-case "arrays: literals, new-array, aref, for-each over arrays, IEnumerable and strings"
  (check-equal? (run-example "arrays.rkt")
                '("6" "2" "1" "2" "3" "4" "5" "6" "21" "3" "True" "False"
                  "10" "20" "30" "a" "b" "7" "8" "6")))

(test-case "enum members and static fields are read by qualified name"
  (check-equal? (run-example "enums.rkt")
                '("Monday" "weekday" "weekend" "True" "weekday" "weekend"
                  "2147483647" "Integer" "3.14159265358979")))

(test-case "array and static field errors are reported at the user's expression"
  (check-regexp-match #rx"type mismatch: expected string, got int32"
                      (tui-error '(define a : |int32[]| (array int32 1 2))
                                 '(for-each [x : string a] (Console.WriteLine x))))
  (check-regexp-match #rx"string is not an array" (tui-error '(let ([s "abc"]) (aref s 0))))
  (check-regexp-match #rx"for-each" (tui-error '(for-each [x 5] (Console.WriteLine x))))
  (check-regexp-match #rx"Funday" (tui-error '(Console.WriteLine System.DayOfWeek.Funday))))

