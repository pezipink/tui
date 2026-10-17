#lang racket/base
; The Tui type system's view of .NET. This module runs at phase 1 (it is required for-syntax by
; the language), where it uses Rangi's lazy CLI reader to look up types and methods in the
; referenced assemblies while the macro expander type checks a program.
;
; Tui types are plain datums so they can be quoted into syntax and stored in bindings:
;   void bool char int32 int64 float64 string object
;   (class "AssemblyName" "Namespace" "Name")   any other reference type
(require racket/list racket/string
         rangi/src/cli/reader rangi/src/cli/index)
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
  (set! opens open-namespaces))

(define (tui-add-reference! r)
  (define p (normalize-reference r))
  (unless (member p references) (set! references (append references (list p)))))

(define (tui-add-open! ns)
  (unless (member ns opens) (set! opens (append opens (list ns)))))

(define (tui-references) references)

; "System.Xml" names a framework reference assembly; anything with a slash or .dll is a path.
(define (normalize-reference r)
  (cond
    [(regexp-match? #rx"[/\\\\]|\\.(dll|exe)$" r) (path->string (path->complete-path r))]
    [else (string-append reference-assemblies-dir r ".dll")]))

;; ---------------------------------------------------------------------------------------------
;; Loaded assemblies. Tui keeps one Rangi reader per path for the life of this phase-1 instance
;; (one module expansion). Rangi caches the indexes it scans on the reader itself.

(struct asm (path name rdr) #:transparent)
(define loaded (make-hash))

(define (load-asm path)
  (hash-ref! loaded path
             (λ ()
               (unless (file-exists? path)
                 (error 'tui "referenced assembly not found: ~a" path))
               (define rdr (read-file path))
               (asm path (read-assembly-meta rdr) rdr))))

; A public top-level TypeDef rid (II.22.37: visibility, the low 3 flag bits, is Public = 1), or #f.
(define (public-typedef a ns name)
  (define rid (find-typedef (asm-rdr a) ns name))
  (and rid (= (bitwise-and (row-column (asm-rdr a) 'typedef rid 'flags) 7) 1) rid))
; Finds a public type by namespace and name in the references: (values asm rid) or (values #f #f).
(define (find-type ns name)
  (let loop ([paths references])
    (cond
      [(null? paths) (values #f #f)]
      [else
       (define a (load-asm (car paths)))
       (define rid (public-typedef a ns name))
       (if rid (values a rid) (loop (cdr paths)))])))

(define (split-full-name full)
  (define parts (string-split full "."))
  (values (string-join (drop-right parts 1) ".") (last parts)))

; A dotted type name as written in source, tried as-is and then under each opened namespace.
; Returns a Tui type or #f.
(define (resolve-clr-type-name full)
  (for/or ([candidate (in-list (cons full (map (λ (o) (string-append o "." full)) opens)))])
    (define-values (ns name) (split-full-name candidate))
    (define-values (a rid) (find-type ns name))
    (and a (normalize-class (asm-name a) ns name))))

(define (normalize-class asm-name ns name)
  (cond
    [(and (equal? ns "System") (equal? name "String")) 'string]
    [(and (equal? ns "System") (equal? name "Object")) 'object]
    [else (list 'class asm-name ns name)]))

(define primitive-aliases
  '((void . void) (bool . bool) (boolean . bool) (char . char)
    (int . int32) (int32 . int32) (long . int64) (int64 . int64)
    (double . float64) (float64 . float64) (string . string) (object . object)))

; A type annotation datum (a symbol) to a Tui type, or #f.
(define (resolve-type-name sym)
  (cond
    [(assq sym primitive-aliases) => cdr]
    [else (resolve-clr-type-name (symbol->string sym))]))

(define value-types '(bool char int32 int64 float64))
(define (value-type? t) (and (memq t value-types) #t))
(define (numeric-type? t) (and (memq t '(int32 int64 float64)) #t))

(define (tui-type->string t)
  (if (and (pair? t) (eq? (car t) 'class))
      (full-name (caddr t) (cadddr t))
      (symbol->string t)))

(define (full-name ns name) (if (equal? ns "") name (string-append ns "." name)))

; Tui type -> Rangi ilasm type syntax (II.23.2.12 element types).
(define (tui-type->il t)
  (case t
    [(bool) 'boolean]
    [(void char int32 int64 float64 string object) t]
    [else (list 'class (owner->il t))]))

; (class asm ns name) -> the (asm Ns.Name) owner form Rangi uses for calls and newobj.
(define (owner->il t)
  (list (string->symbol (cadr t)) (string->symbol (full-name (caddr t) (cadddr t)))))

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
(struct clr-method (owner name static? params ret) #:transparent)

(define (class-of t)
  (case t
    [(string) (list 'class (asm-name (load-asm mscorlib-path)) "System" "String")]
    [(object) (list 'class (asm-name (load-asm mscorlib-path)) "System" "Object")]
    [else t]))

(define (asm-by-name name)
  (for/first ([p (in-list references)] #:when (equal? (asm-name (load-asm p)) name))
    (load-asm p)))

; Signature element (from Rangi's decode-method-sig) -> Tui type, or #f when Tui cannot use it.
(define (sig->tui a s)
  (cond
    [(symbol? s)
     (case s
       [(boolean) 'bool]
       [(void char int32 int64 float64 string object) s]
       [else #f])]
    [(and (pair? s) (eq? (car s) 'class))
     (define target (cadr s))
     (case (car target)
       [(typedef)
        (define rdr (asm-rdr a))
        (normalize-class (asm-name a) (row-column rdr 'typedef (cdr target) 'namespace)
                         (row-column rdr 'typedef (cdr target) 'name))]
       [(typeref)
        ; II.22.38 TypeRef: resolution-scope name namespace. Resolve through the references.
        (define rdr (asm-rdr a))
        (define ns (row-column rdr 'typeref (cdr target) 'namespace))
        (define name (row-column rdr 'typeref (cdr target) 'name))
        (define-values (ta rid) (find-type ns name))
        (and ta (normalize-class (asm-name ta) ns name))]
       [else #f])]
    [else #f]))

; All methods of a type, as clr-methods. Methods whose signatures use types Tui does not model,
; generic methods, and vararg methods are left out. Only public methods are returned.
(define (type-methods t)
  (define a (asm-by-name (cadr t)))
  (define rid (public-typedef a (caddr t) (cadddr t)))
  (define rdr (asm-rdr a))
  (define-values (start end) (typedef-methods rdr rid))
  (for*/list ([mid (in-range start end)]
              [flags (in-value (row-column rdr 'methoddef mid 'flags))]
              ; II.23.1.10 MethodAttributes: MemberAccessMask 0x7, Public 6, Static 0x10
              #:when (= (bitwise-and flags 7) 6)
              [sig (in-value (decode-method-sig (row-column rdr 'methoddef mid 'signature)))]
              #:when (zero? (method-signature-generic-arity sig))
              [params (in-value (map (λ (p) (sig->tui a p)) (method-signature-params sig)))]
              [ret (in-value (sig->tui a (method-signature-ret sig)))]
              #:when (and ret (andmap values params)))
    (clr-method t (row-column rdr 'methoddef mid 'name) (positive? (bitwise-and flags #x10))
                params ret)))

; The base type (II.22.37 Extends) as a Tui class type, or #f.
(define (base-type t)
  (define a (asm-by-name (cadr t)))
  (define ext (row-column (asm-rdr a) 'typedef (public-typedef a (caddr t) (cadddr t)) 'extends))
  (and ext
       (let ([b (sig->tui a (list 'class ext))])
         (and b (class-of b)))))

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
