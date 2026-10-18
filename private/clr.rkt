#lang racket/base
; The Tui type system's view of .NET. This module runs at phase 1 (it is required for-syntax by
; the language). It looks up types and members through Rangi's type layer (src/types), which reads
; the referenced assemblies lazily while the macro expander type checks a program.
;
; Tui types are plain datums so they can be quoted into syntax and stored in bindings:
;   void bool char int32 int64 float64 string object
;   (class "AssemblyName" "Namespace" "Name" (arg ...))   a class, struct or enum, with its type arguments
;   (szarray elem)                                        a single-dimensional array of elem
; Whether a class is a struct or enum comes from its descriptor (see value-type?).
(require racket/string racket/promise racket/list         rangi/src/types/canon rangi/src/types/desc rangi/src/types/reference
         (only-in rangi/src/types/provider make-env make-declaration-provider prefer-local-policy
                  first-match-policy env-type declare-type! declare-member! complete-type!
                  declaration-state [resolve-type-name find-type-name]))
(provide reference-assemblies-dir mscorlib-path
         tui-configure! tui-add-reference! tui-add-open! tui-references tui-opens
         resolve-type-name resolve-ref ref->tui
         generic-instance string->tui-type split-type-args class-of tui-type->il
         tui-type->string value-type? primitive-value-type? numeric-type? coercion
         (struct-out clr-method) find-static-methods find-instance-methods find-constructors
         (struct-out clr-field) type-static-fields find-static-field literal-instructions
         enum-underlying enumerable-element szarray-type? szarray-elem receiver-class
         resolve-overload owner->il base-type supertypes type-methods
         declare-record! reset-local-declarations!
         lambda-placeholder lambda-placeholder? null-placeholder null-placeholder? reference-type?
         delegate-type?
         class-args has-mvar? erase-mvars delegate-param-args bind-type subst-mvars instantiate-method)

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
(define (tui-opens) opens)

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

; Forgets the record types declared so far. Each module's expansion starts with none.
(define (reset-local-declarations!)
  (set! local-provider (make-declaration-provider local-asm))
  (set! current-env #f))

; Declares a record type in the local assembly, so the type checker sees it before any module code
; is checked. fields is a list of (name . tui-type). The record has a constructor that takes every
; field, a private field per name, and get_name and set_name accessors, which read and write it as
; a property.
(define (declare-record! name fields)
  (define r (type-ref local-asm "" name 0 'class '()))
  (when (declaration-state local-provider r)
    (error 'tui "~a is already declared" name))
  (define types (for/list ([f (in-list fields)]) (tui->ref (cdr f))))
  (declare-type! local-provider
                 (type-desc r 'class #f #t (delay (primitive-ref 'object))
                            (delay '()) (delay '()) (delay '())))
  (declare-member! local-provider r
                   (method-desc r ".ctor" #t #f #f #f #f 0 types 'void))
  (for ([f (in-list fields)] [t (in-list types)])
    (define n (car f))
    (declare-member! local-provider r (field-desc r n #f #f t #f #f))
    (declare-member! local-provider r (method-desc r (string-append "get_" n) #t #f #f #f #f 0 '() t))
    (declare-member! local-provider r
                     (method-desc r (string-append "set_" n) #t #f #f #f #f 0 (list t) 'void)))
  (complete-type! local-provider r))

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

; A signature reference as a Tui type, where a method type parameter !!n is (mvar n). A call fixes
; these from its type arguments (see instantiate-method). Other references are as ref->tui.
(define (sig->tui r)
  (cond
    [(type-param? r)
     (and (eq? (type-param-owner r) 'method) (list 'mvar (type-param-index r)))]
    [(and (type-ref? r) (memq (type-ref-kind r) '(class valuetype)) (pair? (type-ref-args r)))
     (define args (map sig->tui (type-ref-args r)))
     (and (andmap values args)
          (list 'class (type-ref-asm r) (type-ref-namespace r) (type-ref-name r) args))]
    [(and (type-ref? r) (eq? (type-ref-kind r) 'szarray))
     (define e (sig->tui (car (type-ref-args r))))
     (and e (list 'szarray e))]
    [else (ref->tui r)]))

; A reference as a Tui type, or #f when Tui does not model it (pointers, byrefs, multi-dimensional
; arrays and type parameters).
(define (ref->tui r)
  (cond
    [(not (type-ref? r)) #f]
    [(hash-ref tui-of-primitive r #f) => values]
    [(eq? (type-ref-kind r) 'szarray)
     (define e (ref->tui (car (type-ref-args r))))
     (and e (list 'szarray e))]
    [(not (memq (type-ref-kind r) '(class valuetype))) #f]
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
    [(szarray-type? t)
     (define e (tui->ref (szarray-elem t)))
     (and e (type-ref #f "" "" 0 'szarray (list e)))]
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
  (or (local-type-ref id arity)
      (find-type-name (the-env) id #:arity arity
                      #:policy (prefer-local-policy local-asm first-match-policy))))

; A record declared in this module. Records have no namespace, which the opened namespaces never
; search, so a bare name is looked up here first. Local types shadow imported ones.
(define (local-type-ref id arity)
  (define text (symbol->string (syntax-e id)))
  (and (= arity 0) (not (regexp-match? #rx"[.]" text))
       (let ([d (env-type (the-env) (type-ref local-asm "" text 0 'class '()))])
         (and d (type-desc-ref d)))))

(define (sub-id id text) (datum->syntax id (string->symbol text) id))

; The canonical reference an identifier names, or #f. Name<A, B> names a generic instance whose
; arguments may themselves be generic (Func<A, B> is Func`2). Multi-argument names need pipes in
; Racket source, e.g. |Func<int32, int32>|, because the reader splits at a comma.
(define (resolve-ref-of id)
  (define sym (syntax-e id))
  (define m (regexp-match #rx"^([^<>]+)<(.+)>$" (symbol->string sym)))
  (cond
    [(assq sym primitive-aliases) => (λ (p) (tui-prim-ref (cdr p)))]
    [m (let* ([args (for/list ([a (in-list (split-type-args (caddr m)))])
                      (resolve-ref-of (sub-id id a)))]
              [base (and (andmap values args)
                         (resolve-ref (sub-id id (cadr m)) (length args)))])
         (and base (andmap values args) (with-args base args)))]
    [else (resolve-ref id 0)]))

; The arguments of a generic name, split at its top-level commas: "A, B<C, D>" is ("A" "B<C, D>").
(define (split-type-args s)
  (let loop ([cs (string->list s)] [depth 0] [cur '()] [acc '()])
    (define (piece) (string-trim (list->string (reverse cur))))
    (cond
      [(null? cs) (reverse (cons (piece) acc))]
      [(and (char=? (car cs) #\,) (zero? depth))
       (loop (cdr cs) depth '() (cons (piece) acc))]
      [else
       (define c (car cs))
       (define d (cond [(char=? c #\<) (add1 depth)] [(char=? c #\>) (sub1 depth)] [else depth]))
       (loop (cdr cs) d (cons c cur) acc)])))

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

; The Tui type of a generic definition instantiated at args (Tui types), e.g.
; (generic-instance "System.Collections.Generic.List" '(int32)), or #f when it cannot be formed.
(define (generic-instance full-name args)
  (define base (resolve-ref (datum->syntax #f (string->symbol full-name)) (length args)))
  (define refs (map tui->ref args))
  (and base (andmap values refs)
       (ref->tui (with-args base refs))))

; A dotted type name in a string, e.g. "System.Console".
(define (resolve-clr-type-name full)
  (resolve-type-name (datum->syntax #f (string->symbol full))))

(define value-types '(bool char int32 int64 float64))

(define (szarray-type? t) (and (pair? t) (eq? (car t) 'szarray)))
(define (szarray-elem t) (cadr t))

; A primitive's instance members are those of System.Object: its value is boxed to dispatch them.
(define (primitive-value-type? t) (and (memq t value-types) #t))

; A primitive value type, or a struct or enum class (told apart by its descriptor).
(define (value-type? t)
  (cond
    [(memq t value-types) #t]
    [(and (pair? t) (eq? (car t) 'class))
     (define d (class-desc t))
     (and d (memq (type-desc-shape d) '(valuetype enum)) #t)]
    [else #f]))

(define (numeric-type? t) (and (memq t '(int32 int64 float64)) #t))

; The type of a bare fn argument during overload resolution: a fn of n parameters, whose delegate
; type is only known once the overload is chosen.
(define (lambda-placeholder n) (list 'lambda n))
(define (lambda-placeholder? t) (and (pair? t) (eq? (car t) 'lambda)))

; The type of a bare null argument during overload resolution: it matches any reference parameter,
; and its code is compiled once the overload is chosen.
(define null-placeholder 'null-arg)
(define (null-placeholder? t) (eq? t null-placeholder))

; A type that a null can have: anything but a value type, void, or an unfixed type parameter.
(define (reference-type? t)
  (not (or (value-type? t) (eq? t 'void) (and (pair? t) (eq? (car t) 'mvar)))))

(define (tui-type->string t)
  (cond
    [(szarray-type? t) (string-append (tui-type->string (szarray-elem t)) "[]")]
    [(and (pair? t) (eq? (car t) 'class))
     (define args (class-args t))
     (string-append (full-name (caddr t) (cadddr t))
                    (if (null? args) "" (format "<~a>" (string-join (map tui-type->string args) ", "))))]
    [(lambda-placeholder? t) (format "fn/~a" (cadr t))]
    [(null-placeholder? t) "null"]
    [else (symbol->string t)]))

(define (full-name ns name) (if (equal? ns "") name (string-append ns "." name)))

; Tui type -> Rangi ilasm type syntax (II.23.2.12 element types).
(define (tui-type->il t)
  (cond
    [(eq? t 'bool) 'boolean]
    [(memq t '(void char int32 int64 float64 string object)) t]
    [(szarray-type? t) (list 'szarray (tui-type->il (szarray-elem t)))]
    [(and (null? (class-args t)) (equal? (cadr t) local-asm))
     (list (il-kind t) (string->symbol (full-name (caddr t) (cadddr t))))]
    [(null? (class-args t)) (list (il-kind t) (owner->il t))]
    [else (owner->il t)]))

; The ilasm keyword of a class type: valuetype for a struct or enum, class otherwise.
(define (il-kind t) (if (value-type? t) 'valuetype 'class))

; (class asm ns name args) -> the owner form Rangi uses for calls and newobj: (asm Ns.Name), or
; (class (asm Ns.Name) (arg ...)) for a generic instance (valuetype for a generic struct). A type
; declared in this module is named without an assembly, as (Ns.Name), like a lambda class.
(define (owner->il t)
  (define name (string->symbol (full-name (caddr t) (cadddr t))))
  (define owner (if (equal? (cadr t) local-asm)
                    (list name)
                    (list (string->symbol (cadr t)) name)))
  (if (null? (class-args t))
      owner
      (list (il-kind t) owner (map tui-type->il (class-args t)))))

;; ---------------------------------------------------------------------------------------------
;; Implicit conversions, used for arguments and returns: (cons cost instructions) or #f.

(define (coercion from to)
  (cond
    [(equal? from to) '(0)]
    ; never (see tui-throw) completes no value, so it converts to anything without code
    [(eq? from 'never) '(0)]
    ; object costs more than a base class or interface, so List<T> prefers IEnumerable<T> to object
    [(and (eq? to 'object) (not (value-type? from)) (not (eq? from 'void))) '(2)]
    [(and (eq? to 'object) (value-type? from)) `(2 (box ,(tui-type->il from)))]
    ; a struct or enum boxes to an interface or base class it implements
    [(and (value-type? from) (pair? to) (member to (supertypes from))) `(1 (box ,(tui-type->il from)))]
    [(and (eq? from 'int32) (eq? to 'int64)) '(1 (conv.i8))]
    [(and (memq from '(int32 int64)) (eq? to 'float64)) '(1 (conv.r8))]
    [(reference-upcast? from to) '(1)]
    [else #f]))

; An implicit reference conversion: a class (or string, or array) to one of its base classes or
; interfaces, with the type arguments those supertypes have in the closed class. The IL needs no
; instruction. A value type is never upcast this way (it is boxed instead, see coercion).
(define (reference-upcast? from to)
  (and (pair? to)
       (or (pair? from) (eq? from 'string))
       (not (value-type? from))
       (and (member to (reference-supertypes from)) #t)))

; The class whose members a value of type t has: an array's are System.Array's, and a string's or
; object's are those of System.String or System.Object (see class-of).
(define (receiver-class t)
  (if (szarray-type? t) (list 'class core-asm "System" "Array" '()) (class-of t)))

; The types a value of type t converts to implicitly: its receiver class and that class's
; supertypes, plus IEnumerable<elem> for an array.
(define (reference-supertypes t)
  (define c (receiver-class t))
  (define e (and (szarray-type? t)
                 (generic-instance "System.Collections.Generic.IEnumerable" (list (szarray-elem t)))))
  (append (cons c (supertypes c)) (if e (list e) '())))

; The element type T of the IEnumerable<T> that t is or implements, or #f. An array's element is
; its own element type.
(define (enumerable-element t)
  (cond
    [(szarray-type? t) (szarray-elem t)]
    [else
     (define c (class-of t))
     (for/first ([s (in-list (cons c (supertypes c)))]
                 #:when (and (pair? s) (eq? (car s) 'class)
                             (equal? (list-ref s 2) "System.Collections.Generic")
                             (equal? (list-ref s 3) "IEnumerable")
                             (= 1 (length (list-ref s 4)))))
       (car (list-ref s 4)))]))

; Every base class and interface of a class type, transitively, as Tui types.
(define (supertypes t)
  (let loop ([todo (direct-supertypes t)] [seen '()])
    (cond
      [(null? todo) seen]
      [(member (car todo) seen) (loop (cdr todo) seen)]
      [else (loop (append (cdr todo) (direct-supertypes (car todo))) (cons (car todo) seen))])))

(define (direct-supertypes t)
  (define d (class-desc t))
  (cond
    [(not d) '()]
    [else
     (define b (force (type-desc-base d)))
     (filter values
             (cons (and b (ref->tui b))
                   (for/list ([i (in-list (force (type-desc-interfaces d)))]) (ref->tui i))))]))

;; ---------------------------------------------------------------------------------------------
;; Methods

; owner is a (class ...) Tui type (string and object are mapped back to System.String/Object).
; arity is the number of the method's own type parameters, and params, ret name them as (mvar n).
; targs is the list of type arguments of a closed generic method, or '() otherwise. il-params and
; il-ret are the signature the call names: the declaring generic definition's types, so a type
; parameter stays !n and a method's stays !!n. A MemberRef on a generic instance is matched against
; the definition (II.22.25).
(struct clr-method (owner name static? arity params ret il-params il-ret targs) #:transparent)

(define (class-of t)
  (case t
    [(string) (list 'class core-asm "System" "String" '())]
    [(object) (list 'class core-asm "System" "Object" '())]
    [else t]))

; The descriptor of a class type, or #f.
(define (class-desc t)
  (define r (and (pair? t) (eq? (car t) 'class) (tui->ref t)))
  (and r (env-type (the-env) r)))


; All public methods of a class type, as clr-methods. Vararg methods, and methods whose signatures
; name types Tui does not model, are left out. A generic method keeps its arity, and its method
; type parameters are (mvar n) until a call instantiates them. The open descriptor gives the
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
                 #:unless (method-desc-vararg? m)
                 [params (in-value (map sig->tui (method-desc-params m)))]
                 [ret (in-value (if (eq? (method-desc-ret m) 'void)
                                    'void
                                    (sig->tui (method-desc-ret m))))]
                 [il-params (in-value (map open-il (method-desc-params o)))]
                 [il-ret (in-value (if (eq? (method-desc-ret o) 'void)
                                       'void
                                       (open-il (method-desc-ret o))))]
                 #:when (and ret (andmap values params) il-ret (andmap values il-params)))
       (clr-method t (method-desc-name m) (method-desc-static? m) (method-desc-arity m)
                   params ret il-params il-ret '()))]))

; A canonical type from an open descriptor, as an ilasm type. A type parameter of the owner is
; !n (II.23.2.12 VAR) and a method's is !!n (MVAR). A generic class instance keeps its open
; arguments. Other constructed types are not modelled, so they give #f.
(define (open-il t)
  (cond
    [(type-param? t)
     (case (type-param-owner t)
       [(type) (string->symbol (format "!~a" (type-param-index t)))]
       [(method) (string->symbol (format "!!~a" (type-param-index t)))]
       [else #f])]
    [(not (has-type-param? t)) (let ([tui (ref->tui t)]) (and tui (tui-type->il tui)))]
    [(and (type-ref? t) (eq? (type-ref-kind t) 'szarray))
     (define e (open-il (car (type-ref-args t))))
     (and e (list 'szarray e))]
    [(and (type-ref? t) (memq (type-ref-kind t) '(class valuetype)))
     (define args (map open-il (type-ref-args t)))
     (and (andmap values args)
          (list (type-ref-kind t)
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

; type-name is a source string like "System.Console" or "Console". A primitive name such as Int32
; names its System class for static members.
(define primitive-classes
  '((bool . "Boolean") (char . "Char") (int32 . "Int32") (int64 . "Int64") (float64 . "Double")))

(define (find-static-methods type-name method-name)
  (define t (resolve-clr-type-name type-name))
  (define owner (and t (static-owner t)))
  (and owner
       (filter (λ (m) (and (clr-method-static? m) (equal? (clr-method-name m) method-name)))
               (type-methods owner))))

; The class whose static members a type names: a primitive's System class, otherwise the type.
(define (static-owner t)
  (cond
    [(assq t primitive-classes) => (λ (p) (list 'class core-asm "System" (cdr p) '()))]
    [else (class-of t)]))

; Instance methods named method-name on t and its base types, most derived first, as a list of
; groups (one group per type in the chain).
(define (find-instance-methods t method-name)
  (let loop ([t (receiver-class t)] [acc '()])
    (cond
      [(not t) (reverse acc)]
      [else
       (define ms (filter (λ (m) (and (not (clr-method-static? m))
                                      (equal? (clr-method-name m) method-name)))
                          (type-methods t)))
       (loop (base-type t) (if (null? ms) acc (cons ms acc)))])))

; Whether t is a delegate type (a class deriving from System.MulticastDelegate).
(define (delegate-type? t)
  (and (pair? t) (eq? (car t) 'class)
       (let loop ([b (base-type t)])
         (and b (or (and (equal? (caddr b) "System") (equal? (cadddr b) "MulticastDelegate"))
                    (loop (base-type b)))))))

; The number of parameters of the Invoke method of delegate type t, or #f if t is not a delegate.
(define (delegate-arity t)
  (and (delegate-type? t)
       (let ([groups (find-instance-methods t "Invoke")])
         (and (pair? groups) (length (clr-method-params (car (car groups))))))))

; A placeholder matches a delegate of the same arity. Its conversion is applied once the overload
; is chosen, so it has no instructions here.
(define (placeholder-coercion from to)
  (and (eqv? (delegate-arity (erase-mvars to)) (cadr from)) (cons 0 '())))

; The coercion of an argument of type from to parameter type to, as coercion, or as a placeholder
; match when from is a bare fn.
(define (arg-coercion from to)
  (cond
    [(lambda-placeholder? from) (placeholder-coercion from to)]
    [(null-placeholder? from) (and (reference-type? to) (cons 0 '()))]
    [else (coercion from to)]))

(define (find-constructors t)
  (filter (λ (m) (and (not (clr-method-static? m)) (equal? (clr-method-name m) ".ctor")))
          (type-methods t)))

; Whether type t mentions a method type parameter that is not yet fixed.
(define (has-mvar? t)
  (cond
    [(and (pair? t) (eq? (car t) 'mvar)) #t]
    [(pair? t) (ormap has-mvar? t)]
    [else #f]))

; The indexes of the method type parameters that t mentions.
(define (mvars-of t)
  (cond
    [(and (pair? t) (eq? (car t) 'mvar)) (list (cadr t))]
    [(pair? t) (append-map mvars-of t)]
    [else '()]))

; t with each unbound (mvar n) replaced by object, so that its shape can be examined.
(define (erase-mvars t)
  (cond
    [(and (pair? t) (eq? (car t) 'mvar)) 'object]
    [(and (pair? t) (eq? (car t) 'class))
     (list 'class (cadr t) (caddr t) (cadddr t) (map erase-mvars (class-args t)))]
    [(szarray-type? t) (list 'szarray (erase-mvars (szarray-elem t)))]
    [else t]))

; For delegate type d, whose arguments may hold unbound (mvar n): the argument index that each
; parameter of its Invoke method is, or #f for a parameter that is not a plain argument.
(define (delegate-param-args d)
  (define groups (find-instance-methods (erase-mvars d) "Invoke"))
  (and (pair? groups)
       (for/list ([il (in-list (clr-method-il-params (car (car groups))))])
         (let ([s (and (symbol? il) (symbol->string il))])
           (and s (regexp-match? #rx"^![0-9]+$" s) (string->number (substring s 1)))))))

; The generic definition a class type instantiates: its assembly, namespace and name.
(define (generic-key t) (list (cadr t) (caddr t) (cadddr t)))

; The supertype of a (a class type, string, array, or primitive, which has none) that has the
; generic definition of p, or #f.
(define (matching-supertype a p)
  (for/first ([s (in-list (reference-supertypes a))]
              #:when (and (pair? s) (eq? (car s) 'class)
                          (equal? (generic-key s) (generic-key p))
                          (= (length (class-args s)) (length (class-args p)))))
    s))

; Matches a parameter type p against an argument type a, binding the method type parameters in
; slots. A generic instance parameter matches the argument's supertype of the same definition, so
; List<int32> fixes !!0 in IEnumerable<!!0>. Returns #f on a conflict. A lambda placeholder binds
; nothing. A type parameter that fixed names (a non-#f entry of fixed) is not bound at all, so an
; argument that differs from it is left to the coercion check.
(define (bind-type p a slots fixed)
  (cond
    [(or (lambda-placeholder? a) (null-placeholder? a)) #t]
    [(and (pair? p) (eq? (car p) 'mvar))
     (define i (cadr p))
     (cond [(vector-ref fixed i) #t]
           [(not (vector-ref slots i)) (vector-set! slots i a) #t]
           [else (equal? (vector-ref slots i) a)])]
    [(and (pair? p) (eq? (car p) 'class) (has-mvar? p))
     (define s (matching-supertype a p))
     ; An argument that is not an instance of p is left to the coercion check once closed.
     (or (not s)
         (for/and ([pp (in-list (class-args p))] [sa (in-list (class-args s))])
           (bind-type pp sa slots fixed)))]
    [(and (szarray-type? p) (has-mvar? p) (szarray-type? a))
     (bind-type (szarray-elem p) (szarray-elem a) slots fixed)]
    [else #t]))

; Whether type parameter i occurs in some bare fn parameter and in no other parameter, so that the
; body of that fn can fix it (see finish-call).
(define (lambda-only? i params arg-types)
  (define (occurs? p) (memv i (mvars-of p)))
  (and (for/or ([p (in-list params)] [a (in-list arg-types)])
         (and (lambda-placeholder? a) (occurs? p)))
       (for/and ([p (in-list params)] [a (in-list arg-types)])
         (or (lambda-placeholder? a) (not (occurs? p))))))

; The type arguments of a generic method call, or #f when they cannot be fixed. explicit may name
; fewer type arguments than the method has, with #f for each one left to infer (Select<int32, _>),
; but not more. Named type arguments are used as given. Otherwise a parameter that is exactly !!n
; takes the type of its argument, and every !!n must be fixed that way with one type. Other
; parameters are checked by coercion once closed. A type argument that only bare fn parameters
; mention is left #f, to be fixed by the fn's body.
(define (infer-targs m arg-types explicit)
  (define k (clr-method-arity m))
  (define params (clr-method-params m))
  (define fixed
    (for/vector #:length k ([i (in-range k)])
      (and (< i (length explicit)) (list-ref explicit i))))
  (define slots (make-vector k #f))
  (and (<= (length explicit) k)
       (for/and ([p (in-list params)] [a (in-list arg-types)]) (bind-type p a slots fixed))
       (for/and ([i (in-range k)])
         (or (vector-ref fixed i) (vector-ref slots i) (lambda-only? i params arg-types)))
       (for/list ([i (in-range k)])
         (or (vector-ref fixed i) (vector-ref slots i)))))

; t with each (mvar n) replaced by the nth of targs, where that is not #f.
(define (subst-mvars t targs)
  (cond
    [(and (pair? t) (eq? (car t) 'mvar)) (or (list-ref targs (cadr t)) t)]
    [(and (pair? t) (eq? (car t) 'class))
     (list 'class (cadr t) (caddr t) (cadddr t) (map (λ (a) (subst-mvars a targs)) (class-args t)))]
    [(szarray-type? t) (list 'szarray (subst-mvars (szarray-elem t) targs))]
    [else t]))

; The closed method at type arguments targs: each (mvar n) becomes the nth argument. A #f
; argument leaves its (mvar n) in place.
(define (instantiate-method m targs)
  (struct-copy clr-method m
               [params (map (λ (p) (subst-mvars p targs)) (clr-method-params m))]
               [ret (subst-mvars (clr-method-ret m) targs)]
               [targs targs]))

; Picks the cheapest applicable overload. groups is a list of candidate lists; earlier groups win
; ties with later ones (a derived type's method hides its base's). explicit is the list of type
; arguments the call names, if any. Returns (values method per-argument-conversion-instructions),
; where method is closed, or raises with a description.
(define (resolve-overload groups arg-types describe [explicit '()])
  (define (close m)
    (cond
      [(zero? (clr-method-arity m)) (and (null? explicit) m)]
      [(infer-targs m arg-types explicit) => (λ (targs) (instantiate-method m targs))]
      [else #f]))
  (define (score m)
    (define c (close m))
    (and c (= (length (clr-method-params c)) (length arg-types))
         (let ([cs (map arg-coercion arg-types (clr-method-params c))])
           (and (andmap values cs)
                (list (apply + (map car cs)) c (map cdr cs) (positive? (clr-method-arity m)))))))
  (let loop ([groups groups])
    (cond
      [(null? groups)
       (error 'tui "no overload of ~a accepts (~a)~a" describe
              (string-join (map tui-type->string arg-types) " ")
              (if (null? explicit)
                  ""
                  (format " with type arguments <~a>"
                          (string-join (map (λ (t) (if t (tui-type->string t) "_")) explicit)
                                       ", "))))]
      [else
       (define scored (for*/list ([m (in-list (car groups))] [s (in-value (score m))] #:when s) s))
       (cond
         [(null? scored) (loop (cdr groups))]
         [else
          (define best (apply min (map car scored)))
          ; Among equally good candidates a non-generic method wins, as in C#.
          (define equal-best (filter (λ (x) (= (car x) best)) scored))
          (define plain (filter (λ (x) (not (list-ref x 3))) equal-best))
          (define winners (if (null? plain) equal-best plain))
          (when (> (length winners) 1)
            (error 'tui "ambiguous call to ~a with (~a)" describe
                   (string-join (map tui-type->string arg-types) " ")))
          (values (cadr (car winners)) (caddr (car winners)))])])))

;; ---------------------------------------------------------------------------------------------
;; Static fields

; A public static field of a class type. A literal (an enum member, or a constant) carries its
; value, which main.rkt emits in place of a load (see literal-instructions).
(struct clr-field (owner name type literal? value) #:transparent)

; The public static fields of a class type, as clr-fields. A field whose type Tui does not model
; is left out, and so is a literal whose value Tui cannot emit.
(define (type-static-fields t)
  (define owner (static-owner t))
  (define d (class-desc owner))
  (cond
    [(not d) '()]
    [else
     (for*/list ([f (in-list (force (type-desc-fields d)))]
                 #:when (and (field-desc-public? f) (field-desc-static? f))
                 [ft (in-value (sig->tui (field-desc-type f)))]
                 #:when (and ft (or (not (field-desc-literal? f))
                                    (literal-instructions ft (field-desc-value f)))))
       (clr-field owner (field-desc-name f) ft (field-desc-literal? f) (field-desc-value f)))]))

; An enum's underlying integral type, from its value__ field, or #f.
(define (enum-underlying t)
  (define d (class-desc t))
  (and d (eq? (type-desc-shape d) 'enum)
       (for/first ([f (in-list (force (type-desc-fields d)))]
                   #:when (and (not (field-desc-static? f)) (equal? (field-desc-name f) "value__")))
         (sig->tui (field-desc-type f)))))

; The instructions that push a literal's value v of Tui type t, or #f when Tui cannot emit it. An
; enum is emitted as its underlying type. A null reference is #f, and is pushed with ldnull.
(define (literal-instructions t v)
  (define base (or (enum-underlying t) t))
  (cond
    [(and (not v) (not (value-type? base))) '((ldnull))]
    [(memq base '(bool char int32)) (list `(ldc.i4 ,(cond [(boolean? v) (if v 1 0)] [else v])))]
    [(eq? base 'int64) (list `(ldc.i8 ,v))]
    [(eq? base 'float64) (list `(ldc.r8 ,(exact->inexact v)))]
    [(and (eq? base 'string) (string? v)) (list `(ldstr ,v))]
    [else #f]))

; The public static field named field-name of the type named type-name, or #f.
(define (find-static-field type-name field-name)
  (define t (resolve-clr-type-name type-name))
  (and t (findf (λ (f) (equal? (clr-field-name f) field-name)) (type-static-fields t))))

; A type named in source text, as the type arguments of a call name it: a primitive alias, a
; dotted class name, or a generic instance such as "System.Collections.Generic.List<int32>".
; Returns the Tui type or #f.
(define (string->tui-type s)
  (define t (string-trim s))
  (cond
    [(assq (string->symbol t) primitive-aliases) => cdr]
    [(regexp-match #rx"^(.+)\\[\\]$" t)
     => (λ (m) (let ([e (string->tui-type (cadr m))]) (and e (list 'szarray e))))]
    [(regexp-match #rx"^([^<]+)<(.*)>$" t)
     => (λ (m)
          (define args (map string->tui-type (split-type-args (caddr m))))
          (and (andmap values args) (generic-instance (string-trim (cadr m)) args)))]
    [else (resolve-clr-type-name t)]))
