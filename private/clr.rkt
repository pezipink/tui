#lang racket/base
; The Tui type system's view of .NET. This module runs at phase 1 (it is required for-syntax by
; the language). It looks up types and members through Rangi's type layer (src/types), which reads
; the referenced assemblies lazily while the macro expander type checks a program.
;
; Tui types are plain datums so they can be quoted into syntax and stored in bindings:
;   void bool char int32 int64 float64 string object
;   (class "AssemblyName" "Namespace" "Name" (arg ...))   any other reference type, with its type arguments
(require racket/string racket/promise
         rangi/src/types/canon rangi/src/types/desc rangi/src/types/reference
         (only-in rangi/src/types/provider make-env make-declaration-provider prefer-local-policy
                  first-match-policy env-type [resolve-type-name find-type-name]))
(provide reference-assemblies-dir mscorlib-path
         tui-configure! tui-add-reference! tui-add-open! tui-references
         resolve-type-name class-of tui-type->il tui-type->string value-type? numeric-type?
         coercion
         (struct-out clr-method) find-static-methods find-instance-methods find-constructors
         resolve-overload owner->il)

(define reference-assemblies-dir
  "C:/Program Files (x86)/Reference Assemblies/Microsoft/Framework/.NETFramework/v4.7.1/")
(define mscorlib-path (string-append reference-assemblies-dir "mscorlib.dll"))

;; ---------------------------------------------------------------------------------------------
;; References and opened namespaces. A Tui module sets these at the start of its expansion
;; (begin-for-syntax in the module body), so they are re-established whenever the module is
;; visited, including when a REPL enters the module's namespace.

(define references '())          ; list of absolute paths, mscorlib first
(define opens '("System"))       ; namespaces searched for short type names

(define (tui-configure! refs open-namespaces)
  (set! references (map normalize-reference refs))
  (set! opens open-namespaces)
  (set! current-env #f))

(define (tui-add-reference! r)
  (define p (normalize-reference r))
  (unless (member p references)
    (set! references (append references (list p)))
    (set! current-env #f)))

(define (tui-add-open! ns)
  (unless (member ns opens)
    (set! opens (append opens (list ns)))
    (set! current-env #f)))

(define (tui-references) references)

; "System.Xml" names a framework reference assembly; anything with a slash or .dll is a path.
(define (normalize-reference r)
  (cond
    [(regexp-match? #rx"[/\\\\]|\\.(dll|exe)$" r) (path->string (path->complete-path r))]
    [else (string-append reference-assemblies-dir r ".dll")]))

;; ---------------------------------------------------------------------------------------------
;; The type environment. Each referenced file is opened once for this phase-1 instance and its
;; reader is kept, since Rangi caches the indexes it scans on the reader. The env is rebuilt when
;; the references or opens change; rebuilding it is cheap because the readers survive.

(define local-asm "TuiModule")
(define local-provider (make-declaration-provider local-asm))

(define ref-providers (make-hash))   ; path -> reference provider
(define (ref-provider path)
  (hash-ref! ref-providers path
             (λ ()
               (unless (file-exists? path)
                 (error 'tui "referenced assembly not found: ~a" path))
               (open-reference-provider path))))

(define current-env #f)
(define (the-env)
  (or current-env
      (let ([e (make-env #:providers (cons local-provider (map ref-provider references))
                         #:opens opens)])
        (set! current-env e)
        e)))

; Conversions between canonical references (Rangi) and Tui types.
(define tui-of-primitive
  (for/hash ([p (in-list '((boolean . bool) (char . char) (int32 . int32) (int64 . int64)
                           (float64 . float64) (string . string) (object . object)))])
    (values (primitive-ref (car p)) (cdr p))))

; The reference a Tui primitive names, or #f for void.
(define (tui-prim-ref t) (primitive-ref (if (eq? t 'bool) 'boolean t)))

; A reference as a Tui type, or #f when Tui does not model it (other valuetypes, arrays, pointers,
; byrefs and type parameters).
(define (ref->tui r)
  (cond
    [(not (type-ref? r)) #f]
    [(hash-ref tui-of-primitive r #f) => values]
    [(not (eq? (type-ref-kind r) 'class)) #f]
    [else
     (define args (map ref->tui (type-ref-args r)))
     (and (andmap values args)
          (list 'class (type-ref-asm r) (type-ref-namespace r) (type-ref-name r) args))]))

; The closed reference a class type names, or #f. A Tui class has no arity suffix, so the open
; type is tried at each arity up to its argument count, highest first. A nested type may take
; fewer of its own arguments than its outer type (II.22.37 generic parameters are per type).
(define (tui->ref t)
  (cond
    [(symbol? t) (tui-prim-ref t)]
    [else
     (define args (map tui->ref (class-args t)))
     (define name (cadddr t))
     (define n (length args))
     (define nested? (regexp-match? #rx"/" name))
     (and (andmap values args)
          (for/or ([k (in-range n -1 -1)] #:when (or nested? (= k n)))
            (let ([d (env-type (the-env) (type-ref (cadr t) (caddr t) name k 'class '()))])
              (and d (with-args (type-desc-ref d) args)))))]))

(define (resolve-ref id arity)
  (find-type-name (the-env) id #:arity arity
                  #:policy (prefer-local-policy local-asm first-match-policy)))

(define (sub-id id text) (datum->syntax id (string->symbol text) id))

; The canonical reference an identifier names, or #f. Name<arg> names a generic instance of one
; argument, and the argument may itself be generic.
(define (resolve-ref-of id)
  (define sym (syntax-e id))
  (define m (regexp-match #rx"^([^<>]+)<(.+)>$" (symbol->string sym)))
  (cond
    [(assq sym primitive-aliases) => (λ (p) (tui-prim-ref (cdr p)))]
    [m (let* ([base (resolve-ref (sub-id id (cadr m)) 1)]
              [arg (and base (resolve-ref-of (sub-id id (caddr m))))])
         (and base arg (with-args base (list arg))))]
    [else (resolve-ref id 0)]))

(define (class-args t) (list-ref t 4))

(define primitive-aliases
  '((void . void) (bool . bool) (boolean . bool) (char . char)
    (int . int32) (int32 . int32) (long . int64) (int64 . int64)
    (double . float64) (float64 . float64) (string . string) (object . object)))

; A type annotation identifier to a Tui type, or #f.
(define (resolve-type-name id)
  (cond
    [(assq (syntax-e id) primitive-aliases) => cdr]
    [else (ref->tui (resolve-ref-of id))]))

; A dotted type name in a string, e.g. "System.Console".
(define (resolve-clr-type-name full)
  (resolve-type-name (datum->syntax #f (string->symbol full))))

(define value-types '(bool char int32 int64 float64))
(define (value-type? t) (and (memq t value-types) #t))
(define (numeric-type? t) (and (memq t '(int32 int64 float64)) #t))

(define (tui-type->string t)
  (cond
    [(and (pair? t) (eq? (car t) 'class))
     (define args (class-args t))
     (string-append (full-name (caddr t) (cadddr t))
                    (if (null? args) "" (format "<~a>" (string-join (map tui-type->string args) ", "))))]
    [else (symbol->string t)]))

(define (full-name ns name) (if (equal? ns "") name (string-append ns "." name)))

; Tui type -> Rangi ilasm type syntax (II.23.2.12 element types).
(define (tui-type->il t)
  (cond
    [(eq? t 'bool) 'boolean]
    [(memq t '(void char int32 int64 float64 string object)) t]
    [(null? (class-args t)) (list 'class (owner->il t))]
    [else (owner->il t)]))

; (class asm ns name args) -> the owner form Rangi uses for calls and newobj: (asm Ns.Name), or
; (class (asm Ns.Name) (arg ...)) for a generic instance.
(define (owner->il t)
  (define owner (list (string->symbol (cadr t)) (string->symbol (full-name (caddr t) (cadddr t)))))
  (if (null? (class-args t))
      owner
      (list 'class owner (map tui-type->il (class-args t)))))

;; ---------------------------------------------------------------------------------------------
;; Implicit conversions, used for arguments and returns: (cons cost instructions) or #f.

(define (coercion from to)
  (cond
    [(equal? from to) '(0)]
    [(and (eq? to 'object) (not (value-type? from)) (not (eq? from 'void))) '(1)]
    [(and (eq? to 'object) (value-type? from)) `(2 (box ,(tui-type->il from)))]
    [(and (eq? from 'int32) (eq? to 'int64)) '(1 (conv.i8))]
    [(and (memq from '(int32 int64)) (eq? to 'float64)) '(1 (conv.r8))]
    [else #f]))

;; ---------------------------------------------------------------------------------------------
;; Methods

; owner is a (class ...) Tui type (string and object are mapped back to System.String/Object).
; params and ret are the substituted Tui types, used for typing. il-params and il-ret are the
; signature the call names: the declaring generic definition's types, so a type parameter stays
; !n. A MemberRef on a generic instance is matched against the definition (II.22.25).
(struct clr-method (owner name static? params ret il-params il-ret) #:transparent)

(define (class-of t)
  (case t
    [(string) (list 'class core-asm "System" "String" '())]
    [(object) (list 'class core-asm "System" "Object" '())]
    [else t]))

; The descriptor of a class type, or #f.
(define (class-desc t)
  (define r (and (pair? t) (tui->ref t)))
  (and r (env-type (the-env) r)))


; All public methods of a class type, as clr-methods. Generic methods, vararg methods, and methods
; whose signatures name types Tui does not model are left out. The open descriptor gives the
; definition's signature, in the same order as the closed one.
(define (type-methods t)
  (define d (class-desc t))
  (cond
    [(not d) '()]
    [else
     (define open
       (if (null? (class-args t))
           d
           (env-type (the-env) (with-args (type-desc-ref d) '()))))
     (for*/list ([(m o) (in-parallel (force (type-desc-methods d)) (force (type-desc-methods open)))]
                 #:when (method-desc-public? m)
                 #:when (zero? (method-desc-arity m))
                 #:unless (method-desc-vararg? m)
                 [params (in-value (map ref->tui (method-desc-params m)))]
                 [ret (in-value (if (eq? (method-desc-ret m) 'void)
                                    'void
                                    (ref->tui (method-desc-ret m))))]
                 [il-params (in-value (map open-il (method-desc-params o)))]
                 [il-ret (in-value (if (eq? (method-desc-ret o) 'void)
                                       'void
                                       (open-il (method-desc-ret o))))]
                 #:when (and ret (andmap values params) il-ret (andmap values il-params)))
       (clr-method t (method-desc-name m) (method-desc-static? m) params ret il-params il-ret))]))

; A canonical type from an open descriptor, as an ilasm type. A type parameter of the owner is
; !n (II.23.2.12 VAR). A generic class instance keeps its open arguments. Other constructed types
; are not modelled, so they give #f.
(define (open-il t)
  (cond
    [(type-param? t)
     (and (eq? (type-param-owner t) 'type)
          (string->symbol (format "!~a" (type-param-index t))))]
    [(not (has-type-param? t)) (let ([tui (ref->tui t)]) (and tui (tui-type->il tui)))]
    [(and (type-ref? t) (eq? (type-ref-kind t) 'class))
     (define args (map open-il (type-ref-args t)))
     (and (andmap values args)
          (list 'class
                (list (string->symbol (type-ref-asm t))
                      (string->symbol (full-name (type-ref-namespace t) (type-ref-name t))))
                args))]
    [else #f]))

(define (has-type-param? t)
  (cond
    [(type-param? t) #t]
    [(type-ref? t) (ormap has-type-param? (type-ref-args t))]
    [else #f]))

; The base type of a class type as a Tui class type, or #f.
(define (base-type t)
  (define d (class-desc t))
  (define b (and d (force (type-desc-base d))))
  (and b (class-of (ref->tui b))))

; type-name is a source string like "System.Console" or "Console".
(define (find-static-methods type-name method-name)
  (define t (resolve-clr-type-name type-name))
  (and t
       (filter (λ (m) (and (clr-method-static? m) (equal? (clr-method-name m) method-name)))
               (type-methods (class-of t)))))

; Instance methods named method-name on t and its base types, most derived first, as a list of
; groups (one group per type in the chain).
(define (find-instance-methods t method-name)
  (let loop ([t (class-of t)] [acc '()])
    (cond
      [(not t) (reverse acc)]
      [else
       (define ms (filter (λ (m) (and (not (clr-method-static? m))
                                      (equal? (clr-method-name m) method-name)))
                          (type-methods t)))
       (loop (base-type t) (if (null? ms) acc (cons ms acc)))])))

(define (find-constructors t)
  (filter (λ (m) (and (not (clr-method-static? m)) (equal? (clr-method-name m) ".ctor")))
          (type-methods t)))

; Picks the cheapest applicable overload. groups is a list of candidate lists; earlier groups win
; ties with later ones (a derived type's method hides its base's). Returns
; (values method per-argument-conversion-instructions) or raises with a description.
(define (resolve-overload groups arg-types describe)
  (define (score m)
    (and (= (length (clr-method-params m)) (length arg-types))
         (let ([cs (map coercion arg-types (clr-method-params m))])
           (and (andmap values cs)
                (cons (apply + (map car cs)) (map cdr cs))))))
  (let loop ([groups groups])
    (cond
      [(null? groups)
       (error 'tui "no overload of ~a accepts (~a)" describe
              (string-join (map tui-type->string arg-types) " "))]
      [else
       (define scored (for*/list ([m (in-list (car groups))] [s (in-value (score m))] #:when s)
                        (cons m s)))
       (cond
         [(null? scored) (loop (cdr groups))]
         [else
          (define best (apply min (map cadr scored)))
          (define winners (filter (λ (x) (= (cadr x) best)) scored))
          (when (> (length winners) 1)
            (error 'tui "ambiguous call to ~a with (~a)" describe
                   (string-join (map tui-type->string arg-types) " ")))
          (values (car (car winners)) (cddr (car winners)))])])))
