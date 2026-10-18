#lang racket/base
; The type-pattern language for macros: define-type-syntax-class, type-match and type-matches?.
; files/type-language.md has the grammar. A pattern is compiled to an expression that builds the
; pattern datum for the solver in type-patterns.rkt. Names are resolved when that expression runs,
; so they see the metadata and the named patterns of the module that uses them.
(require "type-patterns.rkt"
         (for-syntax racket/base racket/list "clr.rkt" "type-patterns.rkt"
                     (only-in syntax/parse define-syntax-class pattern attribute)))

(provide define-type-syntax-class type-match type-matches?)

;; ---------------------------------------------------------------------------------------------
;; Everything below runs at phase 1: the compiler, and the helpers that compiled patterns and
;; the generated syntax classes call from the using module's phase 1.

(begin-for-syntax
  ; The pattern builders pat-bare, pat-gen and pat-open are in type-patterns.rkt: the expansion
  ; calls them, so they must exist at phase 0 of this module as well.

  ; The name of a binder identifier such as ?T, without the ?, or #f when it is not a binder.
  (define-values (binder-name)
    (λ (id)
      (define s (symbol->string (syntax-e id)))
      (and (> (string-length s) 1) (char=? (string-ref s 0) #\?)
           (string->symbol (substring s 1)))))

  (define-values (list-of)
    (λ (stx what)
      (or (syntax->list stx)
          (raise-syntax-error 'type-pattern (format "expected a list of ~a" what) stx))))

  (define-values (head-name)
    (λ (stx)
      (and (pair? (syntax-e stx))
           (let ([h (car (syntax-e stx))])
             (and (identifier? h) (syntax-e h))))))

  (define-values (check-arity)
    (λ (stx n)
      (define l (cdr (syntax->list stx)))
      (unless (= (length l) n)
        (raise-syntax-error 'type-pattern
                            (format "~a takes ~a argument(s)" (syntax-e (car (syntax->list stx))) n)
                            stx))))

  (define-values (colon?)
    (λ (x) (and (identifier? x) (eq? (syntax-e x) ':))))

  ; locals: the member type parameters in scope (symbols). binders: a box of the binder
  ; identifiers collected so far, in order and unique by name.
  (define-values (compile-pattern)
    (λ (stx locals binders)
      (define (sub p) (compile-pattern p locals binders))
      (cond
        [(identifier? stx)
         (define name (binder-name stx))
         (cond
           [(eq? (syntax-e stx) '_) #''(any)]
           [(and name (memq name locals)) #`(list 'bind (quote #,(cons 'local name)))]
           [name
            (unless (for/or ([b (in-list (unbox binders))]) (eq? (syntax-e b) name))
              (set-box! binders (append (unbox binders) (list (datum->syntax stx name)))))
            #`(list 'bind (quote #,name))]
           [(type-pattern-defined? (syntax-e stx)) #`(list 'named (quote #,(syntax-e stx)))]
           [else #`(pat-bare (quote-syntax #,stx))])]
        [(head-name stx)
         => (λ (h)
              (define l (syntax->list stx))
              (define args (cdr l))
              (case h
                [(and or)
                 #`(list (quote #,h) (list #,@(for/list ([p (in-list args)]) (sub p))))]
                [(not opt szarray)
                 (check-arity stx 1)
                 #`(list (quote #,h) #,(sub (car args)))]
                [(<:)
                 (check-arity stx 1)
                 #`(list 'sub #,(sub (car args)))]
                [(exactly)
                 (check-arity stx 1)
                 #`(list 'exactly (lambda () #,(car args)))]
                [(?)
                 (unless (<= 1 (length args) 2)
                   (raise-syntax-error 'type-pattern "? takes a predicate and an optional pattern" stx))
                 #`(list 'pred (lambda () #,(car args))
                         #,(if (pair? (cdr args)) (sub (cadr args)) #'#f))]
                [(app)
                 (check-arity stx 2)
                 #`(list 'app (lambda () #,(car args)) #,(sub (cadr args)))]
                [(has)
                 #`(list 'has (list #,@(for/list ([m (in-list args)])
                                         (compile-member m locals binders))))]
                [(open)
                 (check-arity stx 1)
                 (unless (identifier? (car args))
                   (raise-syntax-error 'type-pattern "open takes a generic type name" stx))
                 #`(pat-open (quote-syntax #,(car args)))]
                [else
                 #`(pat-gen (quote-syntax #,(car l)) (list #,@(for/list ([p (in-list args)]) (sub p))))]))]
        [else (raise-syntax-error 'type-pattern "bad type pattern" stx)])))

  ; A member: (.Name [?T ...] (param ...) : ret), (.Name (param ...) : ret), or (.ctor (param ...)).
  ; Any of them may be preceded by static.
  (define-values (compile-member)
    (λ (m locals binders)
      (define l0 (list-of m "member"))
      (define static? (and (identifier? (car l0)) (eq? (syntax-e (car l0)) 'static)))
      (define l1 (if static? (cdr l0) l0))
      (unless (and (pair? l1) (identifier? (car l1)))
        (raise-syntax-error 'type-pattern "expected a member such as (.Name (param ...) : ret)" m))
      (define mname (symbol->string (syntax-e (car l1))))
      (unless (and (> (string-length mname) 1) (char=? (string-ref mname 0) #\.))
        (raise-syntax-error 'type-pattern "a member name starts with ." (car l1)))
      (define clr-name (if (equal? mname ".ctor") ".ctor" (substring mname 1)))
      (define rest (cdr l1))
      (define n (length rest))
      (define-values (tps-stx params-stx ret-stx)
        (cond
          [(and (= n 4) (colon? (caddr rest))) (values (car rest) (cadr rest) (cadddr rest))]
          [(and (= n 3) (colon? (cadr rest))) (values #f (car rest) (caddr rest))]
          [(and (= n 1) (equal? clr-name ".ctor")) (values #f (car rest) #f)]
          [else (raise-syntax-error 'type-pattern
                                    "expected a member such as (.Name (param ...) : ret)" m)]))
      (when (and (equal? clr-name ".ctor") ret-stx)
        (raise-syntax-error 'type-pattern "a constructor has no return type" m))
      (define tps
        (if tps-stx
            (for/list ([t (in-list (list-of tps-stx "type parameters"))])
              (or (binder-name t)
                  (raise-syntax-error 'type-pattern "a type parameter is written ?T" t)))
            '()))
      (define inner (append tps locals))
      (define param-exprs
        (for/list ([p (in-list (list-of params-stx "parameters"))])
          (compile-pattern p inner binders)))
      (define ret-expr (if ret-stx (compile-pattern ret-stx inner binders) #'#f))
      #`(list 'method #,static? #,clr-name (quote #,tps) (list #,@param-exprs) #,ret-expr)))

  ; The clauses of type-match, from the last clause back to the first.
  (define-values (type-match-clauses)
    (λ (tt clauses)
      (cond
        [(null? clauses) #`(error 'type-match "no pattern matches ~e" #,tt)]
        [else
         (define parts (list-of (car clauses) "a clause"))
         (unless (and (pair? parts) (pair? (cdr parts)))
           (raise-syntax-error 'type-match "a clause is [pattern body ...]" (car clauses)))
         (define head (car parts))
         (cond
           [(and (identifier? head) (eq? (syntax-e head) 'else))
            #`(let () #,@(cdr parts))]
           [else
            (define binders (box '()))
            (define code (compile-pattern head '() binders))
            (define rest (type-match-clauses tt (cdr clauses)))
            #`(let ([sol (match-type-pattern #,code #,tt)])
                (if sol
                    (let (#,@(for/list ([b (in-list (unbox binders))])
                               #`[#,b (solution-ref sol (quote #,(syntax-e b)))]))
                      #,@(cdr parts))
                    #,rest))])])))

  (define-values (build-syntax-class)
    (λ (name-stx desc-stx pat)
      (define binders (box '()))
      (define code (compile-pattern pat '() binders))
      (define bs (unbox binders))
      (define bnames (map syntax-e bs))
      (when (memq 'Type bnames)
        (raise-syntax-error 'define-type-syntax-class "a binder cannot be named Type" pat))
      (define sym (syntax-e name-stx))
      (define desc (if desc-stx (syntax-e desc-stx) (format "~s" (syntax->datum pat))))
      (define type-id (datum->syntax name-stx 'Type))
      (define attr-clauses
        (append* (for/list ([b (in-list bs)] [bsym (in-list bnames)])
                   (list #'#:attr b #`(solution-ref (attribute sol) (quote #,bsym))))))
      #`(begin-for-syntax
          (register-type-pattern! (quote #,sym) (quote #,bnames) (lambda () #,code))
          (define-syntax-class #,name-stx
            #:description #,desc
            #:attributes (#,type-id #,@bs)
            (pattern e
              #:attr #,type-id (tui-expr-type #'e)
              #:attr sol (match-type-pattern (lookup-type-pattern (quote #,sym)) (attribute #,type-id))
              #:fail-unless (attribute sol) #,(format "expected ~a" desc)
              #,@attr-clauses)))))
  )

;; (define-type-syntax-class Name [#:description "text"] pattern)
; Defines a syntax class Name. A pattern variable x of class Name has x.Type, the type of the
; expression, and one attribute for each ?binder, such as x.Elem for ?Elem.
(define-syntax (define-type-syntax-class stx)
  (syntax-case stx ()
    [(_ name #:description desc pat) (build-syntax-class #'name #'desc #'pat)]
    [(_ name pat) (build-syntax-class #'name #f #'pat)]
    [_ (raise-syntax-error 'define-type-syntax-class
                           "expected (define-type-syntax-class Name [#:description \"text\"] pattern)"
                           stx)]))

;; (type-match t [pattern body ...] ... [else body ...])
; The body of the first clause whose pattern matches the type t. Each ?binder is bound in its body.
(define-syntax (type-match stx)
  (syntax-case stx ()
    [(_ t clause ...)
     #`(let ([tt t])
         #,(type-match-clauses #'tt (syntax->list #'(clause ...))))]
    [_ (raise-syntax-error 'type-match "expected (type-match t [pattern body ...] ...)" stx)]))

;; (type-matches? t pattern)
; Whether the type t matches the pattern. The pattern may not have binders.
(define-syntax (type-matches? stx)
  (syntax-case stx ()
    [(_ t pat)
     (let ([binders (box '())])
       (define code (compile-pattern #'pat '() binders))
       (unless (null? (unbox binders))
         (raise-syntax-error 'type-matches? "a pattern with binders cannot be used here; use type-match"
                             #'pat))
       #`(let ([tt t]) (and (match-type-pattern #,code tt) #t)))]
    [_ (raise-syntax-error 'type-matches? "expected (type-matches? t pattern)" stx)]))
