#lang racket/base
(require rackunit racket/list racket/port racket/runtime-path (file "../private/meta-server.rkt"))

(define file "meta-test.tui")
(define-runtime-path linq "../examples/linq.rkt")
(define src
  (string-append
   "#lang tui\n"
   "(define counter : int 0)\n"
   "(define (f [n : int]) : int (let ([n (+ n 1)]) n))\n"
   "(let ([names (list-of \"ada\" \"grace\")])\n"
   "  (Console.WriteLine (.get_Count names)))\n"))

(define (offset-of s) (caar (regexp-match-positions (regexp-quote s) src)))
(define (reply op . fields)
  (handle-request (apply hasheq 'id 1 'op op 'file file fields)))
(define (names-at s)
  (map (λ (e) (hash-ref e 'name)) (hash-ref (reply "names" 'offset (offset-of s)) 'names)))

(test-case "scopes record the binders visible at a position"
  (check-true (hash-ref (reply "scopes" 'text src) 'ok))
  (check-equal? (names-at "(.get_Count") '("names" "counter" "f"))
  ; the inner n shadows the parameter n inside the let
  (check-equal? (hash-ref (findf (λ (e) (equal? (hash-ref e 'name) "n"))
                                 (hash-ref (reply "names" 'offset (offset-of "(+ n 1)")) 'names))
                        'type)
                "int32"))

(test-case "function records are marked as functions"
  (define f (findf (λ (e) (equal? (hash-ref e 'name) "f"))
                   (hash-ref (reply "names" 'offset (offset-of "(.get_Count")) 'names)))
  (check-equal? (hash-ref f 'kind) "function")
  (check-equal? (hash-ref f 'type) "(int32) -> int32"))

(test-case "members of a generic value include its properties and methods"
  (define ms (hash-ref (reply "members" 'offset (offset-of "(.get_Count") 'receiver "names") 'members))
  (check-not-false (findf (λ (e) (and (equal? (hash-ref e 'name) "Count")
                                      (equal? (hash-ref e 'kind) "property")
                                      (equal? (hash-ref e 'type) "int32")))
                          ms))
  (check-not-false (findf (λ (e) (and (equal? (hash-ref e 'name) "Add")
                                      (equal? (hash-ref e 'signature) "(string) -> void")))
                          ms))
  (check-false (findf (λ (e) (regexp-match? #rx"^(set_|op_|get_Count$)" (hash-ref e 'name))) ms))
  (check-not-false (findf (λ (e) (equal? (hash-ref e 'name) "get_Item")) ms)))

(test-case "relative requires resolve against the file, not the helper's working directory"
  (define linq-text (call-with-input-file linq port->string))
  (define r
    (parameterize ([current-directory (find-system-path 'temp-dir)])
      (handle-request (hasheq 'id 1 'op "scopes" 'file (path->string linq) 'text linq-text))))
  (check-true (hash-ref r 'ok) (format "scopes failed: ~a" (hash-ref r 'error #f))))

(test-case "a type receiver lists static members"
  (define ms (hash-ref (reply "members" 'offset (offset-of "Console") 'receiver "Console") 'members))
  (check-not-false (findf (λ (e) (equal? (hash-ref e 'name) "Out")) ms))
  (check-not-false (findf (λ (e) (equal? (hash-ref e 'name) "ReferenceEquals")) ms)
                   "a static member inherited from Object")
  (check-false (findf (λ (e) (equal? (hash-ref e 'name) "Count")) ms)))

(test-case "types lists public types of the references and marks opened namespaces"
  (define ts (hash-ref (reply "types") 'types))
  (check-not-false (findf (λ (e) (and (equal? (hash-ref e 'name) "Console")
                                      (equal? (hash-ref e 'namespace) "System")
                                      (hash-ref e 'opened)))
                          ts))
  (check-not-false (findf (λ (e) (and (equal? (hash-ref e 'name) "List")
                                      (equal? (hash-ref e 'arity) 1)))
                          ts)))

(test-case "a source that fails to expand keeps the last good scopes"
  (define r (reply "scopes" 'text "#lang tui\n(let ([x ("))
  (check-false (hash-ref r 'ok))
  (check-true (hash-ref r 'kept))
  (check-equal? (names-at "(.get_Count") '("names" "counter" "f")))

(test-case "members of a value include the members inherited from Object"
  (define (names-of receiver s)
    (map (λ (e) (hash-ref e 'name))
         (hash-ref (reply "members" 'offset (offset-of s) 'receiver receiver) 'members)))
  (define generic (names-of "names" "(.get_Count"))
  (for ([n '("ToString" "GetHashCode" "Equals" "GetType")])
    (check-not-false (member n generic) (format "~a on a List<string> value" n)))
  (check-equal? (count (λ (n) (equal? n "ToString")) generic) 1 "an overridden method is listed once")
  (define number (names-of "counter" "(define (f"))
  (check-not-false (member "ToString" number) "int32 is a value type, so it inherits ToString")
  (check-not-false (member "GetHashCode" number)))

(test-case "members of a numeric literal receiver are its instance members"
  (define (names-of receiver)
    (map (λ (e) (hash-ref e 'name))
         (hash-ref (reply "members" 'offset (offset-of "(.get_Count") 'receiver receiver) 'members)))
  (define int (names-of "1"))
  (check-not-false (member "ToString" int))
  (check-not-false (member "GetType" int))
  (check-false (member "Parse" int) "a static member is not offered on a value")
  (define dbl (names-of "1.5"))
  (check-not-false (member "ToString" dbl))
  (check-not-false (member "GetHashCode" dbl))
  (check-equal? (names-of "no-such-type-or-value") '()))

(test-case "a static field of a class is a field, and an enum's literal fields are its members"
  (define (entry receiver name)
    (findf (λ (e) (equal? (hash-ref e 'name) name))
           (hash-ref (reply "members" 'offset (offset-of "(.get_Count") 'receiver receiver) 'members)))
  (check-equal? (hash-ref (entry "System.Int32" "MaxValue") 'kind) "field")
  (check-equal? (hash-ref (entry "System.Int32" "MaxValue") 'type) "int32")
  (check-equal? (hash-ref (entry "System.DayOfWeek" "Monday") 'kind) "enum-member")
  (check-false (entry "System.DayOfWeek" "value__") "the underlying value__ field is not a member"))

(test-case "an array value lists its Length property and the members of System.Array"
  (define arr-file "arr-test.tui")
  (define arr-src "#lang tui\n(let ([xs (array int32 1 2 3)])\n  (aref xs 0))\n")
  (handle-request (hasheq 'id 4 'op "scopes" 'file arr-file 'text arr-src))
  (define ms
    (hash-ref (handle-request
               (hasheq 'id 5 'op "members" 'file arr-file 'receiver "xs"
                       'offset (caar (regexp-match-positions #rx"\\(aref" arr-src))))
              'members))
  (check-not-false (findf (λ (e) (and (equal? (hash-ref e 'name) "Length")
                                      (equal? (hash-ref e 'kind) "property")
                                      (equal? (hash-ref e 'type) "int32")))
                          ms))
  (check-not-false (findf (λ (e) (equal? (hash-ref e 'name) "GetType")) ms)))

(test-case "requests for an unknown file or op are reported, not raised"
  (check-false (hash-ref (handle-request (hasheq 'id 2 'op "names" 'file "never-sent.tui" 'offset 0)) 'ok))
  (check-false (hash-ref (handle-request (hasheq 'id 3 'op "bogus" 'file file)) 'ok)))
