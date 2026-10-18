#lang racket/base
; The runtime of the type-pattern language (files/type-language.md describes the grammar). A
; pattern is a datum built at phase 1 by the expansion of define-type-syntax-class, type-match
; and type-matches?. solve returns every way a pattern matches a Tui type, each as an immutable
; hash from binder name to type. The first solution is the match.
(require racket/list
         "clr.rkt")

(provide register-type-pattern! lookup-type-pattern type-pattern-defined? type-pattern-binders
         set-type-of-hook! tui-expr-type tui-type pat-bare pat-gen pat-open
         solve match-type-pattern solution-ref)

;; ---------------------------------------------------------------------------------------------
;; Named patterns and the type checker hook

; name -> (vector binders make pattern). A pattern is built on its first lookup, not when it is
; registered: building it reads the metadata, which the module using it configures later.
(define registry (make-hasheq))

(define (register-type-pattern! name binders make)
  (hash-set! registry name (vector binders make #f)))

(define (type-pattern-defined? name) (hash-has-key? registry name))

(define (type-pattern-binders name) (vector-ref (hash-ref registry name) 0))

(define (lookup-type-pattern name)
  (define entry (hash-ref registry name #f))
  (unless entry (error 'type-pattern "no type pattern named ~a" name))
  (or (vector-ref entry 2)
      (let ([p ((vector-ref entry 1))])
        (vector-set! entry 2 p)
        p)))

; A bare name in a pattern, as the expansion sees it: a type pattern defined by
; define-type-syntax-class, or else a type. Resolved when the expansion runs.
(define (pat-bare id)
  (define sym (syntax-e id))
  (cond
    [(type-pattern-defined? sym) (list 'named sym)]
    [(resolve-type-name id) => (λ (t) (list 'is t))]
    [else (raise-syntax-error 'type-pattern "unknown type or type pattern" id)]))

(define (class-datum? t) (and (pair? t) (eq? (car t) 'class)))

; A generic class applied to pats, such as (List ?T).
(define (pat-gen id pats)
  (define r (resolve-ref id (length pats)))
  (define t (and r (ref->tui r)))
  (unless (class-datum? t)
    (raise-syntax-error 'type-pattern (format "unknown generic type with ~a argument(s)" (length pats)) id))
  (list 'gen (cadr t) (caddr t) (cadddr t) pats))

; An open generic class, such as (open List), of any arity.
(define (pat-open id)
  (or (for/or ([n (in-range 1 9)])
        (let* ([r (resolve-ref id n)]
               [t (and r (ref->tui r))])
          (and (class-datum? t) (list 'open (cadr t) (caddr t) (cadddr t)))))
      (raise-syntax-error 'type-pattern "unknown generic type" id)))

; The type checker lives in main.rkt, which requires this module, so it is installed here.
(define type-of-hook (box #f))
(define (set-type-of-hook! f) (set-box! type-of-hook f))

; The Tui type of a typed expression (syntax), as a type checker answer.
(define (tui-expr-type stx)
  (define f (unbox type-of-hook))
  (unless f (error 'tui-expr-type "the type checker is not installed"))
  (f stx))

; The Tui type named by a string such as "System.Text.StringBuilder" or "int32", or an error.
(define (tui-type name)
  (or (string->tui-type name) (error 'tui-type "unknown type ~s" name)))

;; ---------------------------------------------------------------------------------------------
;; Matching

(define (class-type? t) (and (pair? t) (eq? (car t) 'class)))
(define (mvar? t) (and (pair? t) (eq? (car t) 'mvar)))

; A primitive has no class of its own in the Tui type language, so its members come from the
; System class it is named by. The class of string and object gives the core assembly.
(define prim-class-names
  (hasheq 'bool "Boolean" 'char "Char" 'int32 "Int32" 'int64 "Int64" 'float64 "Double"
          'string "String" 'object "Object"))

; The class whose members and base types a type has, or #f. The core assembly is read on demand,
; because the metadata is configured only once the module that uses it is expanded.
(define (owner-class t)
  (cond
    [(and (symbol? t) (hash-ref prim-class-names t #f))
     => (λ (name) (list 'class (cadr (class-of 'string)) "System" name '()))]
    [(class-type? t) t]
    [else #f]))

; The base classes and interfaces of t, most derived first.
(define (super-types t)
  (define c (owner-class t))
  (if c (supertypes c) '()))

(define (strip-locals env)
  (for/hash ([(k v) (in-hash env)] #:unless (pair? k)) (values k v)))

; A binder. A declared member type parameter is pre-bound to its (mvar n) and so only equals it.
(define (bind-key k t env)
  (cond
    [(hash-has-key? env k) (if (equal? (hash-ref env k) t) (list env) '())]
    [(mvar? t) '()]
    [else (list (hash-set env k t))]))

(define (solve-all pats types env)
  (for/fold ([sols (list env)]) ([p (in-list pats)] [ty (in-list types)])
    (append-map (λ (e) (solve p ty e)) sols)))

; The method and constructor members of the type t that a has-member pattern can match.
; A member is (method static? name tparams params ret). A constructor is named ".ctor" and has
; ret #f. Constructors are not inherited, so only the class itself is searched for them.
(define (solve-member m t env)
  (define-values (static? name tparams params ret)
    (apply values (cdr m)))
  (define owners
    (let ([c (owner-class t)])
      (cond
        [(not c) '()]
        [(equal? name ".ctor") (list c)]
        [else (cons c (supertypes c))])))
  (define candidates
    (for*/list ([o (in-list owners)]
                [cm (in-list (type-methods o))]
                #:when (equal? (clr-method-name cm) name)
                #:when (eq? (and (clr-method-static? cm) #t) static?)
                #:when (= (clr-method-arity cm) (length tparams))
                #:when (= (length (clr-method-params cm)) (length params)))
      cm))
  (remove-duplicates
   (append*
    (for/list ([cm (in-list candidates)])
      (define env1
        (for/fold ([e env]) ([T (in-list tparams)] [i (in-naturals)])
          (hash-set e (cons 'local T) (list 'mvar i))))
      (define after-params (solve-all params (clr-method-params cm) env1))
      (define after-ret
        (if ret
            (append-map (λ (e) (solve ret (clr-method-ret cm) e)) after-params)
            after-params))
      (map strip-locals after-ret)))))

(define (solve-node p t env)
  (case (car p)
    [(any) (list env)]
    [(bind) (bind-key (cadr p) t env)]
    [(is) (if (equal? (cadr p) t) (list env) '())]
    [(named) (solve (lookup-type-pattern (cadr p)) t env)]
    [(gen)
     (define-values (asm ns name pats) (apply values (cdr p)))
     (if (and (class-type? t) (equal? (cadr t) asm) (equal? (caddr t) ns)
              (equal? (cadddr t) name) (= (length (class-args t)) (length pats)))
         (solve-all pats (class-args t) env)
         '())]
    [(open)
     (define-values (asm ns name) (apply values (cdr p)))
     (if (and (class-type? t) (equal? (cadr t) asm) (equal? (caddr t) ns)
              (equal? (cadddr t) name) (pair? (class-args t)))
         (list env)
         '())]
    [(szarray)
     (if (and (pair? t) (eq? (car t) 'szarray))
         (solve (cadr p) (cadr t) env)
         '())]
    [(sub) (append-map (λ (s) (solve (cadr p) s env)) (super-types t))]
    [(and) (for/fold ([sols (list env)]) ([q (in-list (cadr p))])
             (append-map (λ (e) (solve q t e)) sols))]
    [(or) (or (for/or ([q (in-list (cadr p))])
                (let ([s (solve q t env)]) (and (pair? s) s)))
              '())]
    [(not) (if (null? (solve (cadr p) t env)) (list env) '())]
    [(opt) (let ([s (solve (cadr p) t env)]) (if (null? s) (list env) s))]
    [(exactly) (if (equal? ((cadr p)) t) (list env) '())]
    [(pred)
     (let ([f ((cadr p))])
       (cond
         [(not (f t)) '()]
         [(caddr p) (solve (caddr p) t env)]
         [else (list env)]))]
    [(app)
     (let ([r (((cadr p)) t)])
       (if r (solve (caddr p) r env) '()))]
    [(has) (for/fold ([sols (list env)]) ([m (in-list (cadr p))])
             (append-map (λ (e) (solve-member m t e)) sols))]
    [else (error 'type-pattern "unknown pattern ~s" p)]))

; Every solution of pattern p against the type t, extending env. Leaf patterns do not match a
; declared member type parameter (an mvar), which only a binder can match.
(define (solve p t env)
  (if (and (mvar? t) (not (memq (car p) '(any bind not opt and or))))
      '()
      (solve-node p t env)))

; The first solution of pattern p against t, as its binder environment, or #f.
(define (match-type-pattern p t)
  (define sols (solve p t (hash)))
  (and (pair? sols) (car sols)))

(define (solution-ref env k) (hash-ref env k #f))
