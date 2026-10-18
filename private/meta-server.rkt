#lang racket/base
; The metadata helper for editors. It is a long-lived process that reads one JSON request per line
; on stdin and writes one JSON reply per line on stdout. Requests are:
;   scopes   {"op":"scopes","id":n,"file":path,"text":source}
;            expands the source and keeps its scopes. A source that fails to expand keeps the scopes
;            of its last good expansion.
;   names    {"op":"names","id":n,"file":path,"offset":i}
;            the values and types visible at offset i
;   types    {"op":"types","id":n,"file":path}
;            the public types of the file's references, with whether their namespace is opened
;   members  {"op":"members","id":n,"file":path,"offset":i,"receiver":text}
;            the members of a visible value, or the static members of the type named by receiver
; Offsets are 0-based character offsets into the source. Scope positions are 1-based.
(require json racket/list racket/string
         (only-in rangi/src/cli/reader read-file get-public-typedefs)
         (only-in "clr.rkt" tui-configure! type-methods base-type string->tui-type class-of tui-type->string
                  clr-method-name clr-method-static? clr-method-params clr-method-ret))
(provide handle-request main)

; A scope record as logged by the compiler: (vector name type-text position span start end).
(define (rec-name r) (vector-ref r 0))
(define (rec-type r) (vector-ref r 1))
(define (rec-start r) (vector-ref r 4))
(define (rec-end r) (vector-ref r 5))

(struct snapshot (recs refs opens))
(define snapshots (make-hash))  ; file -> snapshot of its last good expansion
(define configured #f)          ; (cons refs opens) the phase-0 clr.rkt is configured with
(define tui-ns #f)              ; one namespace for every expansion
(define scopes-rcv #f)          ; receives one tui-scopes log message per expansion

(define (init!)
  (set! tui-ns (make-base-namespace))
  (set! scopes-rcv (make-log-receiver (current-logger) 'info 'tui-scopes)))

; The data of the last tui-scopes message received, (source records refs opens), or #f.
(define (drain-scopes!)
  (let loop ([last #f])
    (define msg (sync/timeout 0 scopes-rcv))
    (if msg (loop (vector-ref msg 2)) last)))

; Expands the source as the body of a module in the tui language. The #lang line, if any, is
; skipped, and the forms keep their positions in the source.
(define (expand-source file text)
  (unless tui-ns (init!))
  (define port (open-input-string text))
  (when (regexp-match? #rx"^#lang" text) (read-line port))
  (define forms
    (let loop ([acc '()])
      (define f (read-syntax file port))
      (if (eof-object? f) (reverse acc) (loop (cons f acc)))))
  (define mod (datum->syntax #f (list* 'module 'm 'tui forms) (list file 1 0 1 #f)))
  (drain-scopes!)
  ; Relative requires such as (require "fnlib.rkt") resolve against the file's directory, not the
  ; helper's working directory.
  (define-values (dir _name _must-be-dir?) (split-path (path->complete-path file)))
  (parameterize ([current-namespace tui-ns]
                 [current-load-relative-directory dir])
    (expand mod))
  (define data (drain-scopes!))
  (unless data (error 'scopes "the expansion logged no scopes for ~a" file))
  (hash-set! snapshots file (snapshot (cadr data) (caddr data) (cadddr data))))

(define (snapshot-for file)
  (or (hash-ref snapshots file #f)
      (error 'meta "no scopes for ~a; send a scopes request first" file)))

; The phase-0 clr.rkt answers member queries, so it is configured as the module that was expanded.
(define (configure! snap)
  (define key (cons (snapshot-refs snap) (snapshot-opens snap)))
  (unless (equal? key configured)
    (tui-configure! (snapshot-refs snap) (snapshot-opens snap))
    (set! configured key)))

; The records visible at a 0-based offset, innermost first. A name shadowed by an inner binder
; appears once, as the inner binder.
(define (visible snap offset)
  (define pos (add1 offset))
  (define in-scope
    (for/list ([r (in-list (snapshot-recs snap))]
               #:when (and (<= (rec-start r) pos) (< pos (rec-end r))))
      r))
  (remove-duplicates (sort in-scope > #:key rec-start) #:key rec-name))

(define (record-kind r)
  (if (regexp-match? #rx"->" (rec-type r)) "function" "value"))

; The public top-level types of a reference, as (name arity namespace). Generic types are named
; `Name`arity in metadata; the arity is split off.
(define typedef-cache (make-hash))
(define (reference-types path)
  (hash-ref! typedef-cache path
             (λ ()
               (for/list ([td (in-list (get-public-typedefs (read-file path)))])
                 (define m (regexp-match #rx"^(.*)`([0-9]+)$" (car td)))
                 (list (if m (cadr m) (car td))
                       (if m (string->number (caddr m)) 0)
                       (cdr td))))))

(define (type-text t)
  (with-handlers ([exn:fail? (λ (_) "?")]) (tui-type->string t)))

; A member of a clr-method, as a JSON object, or #f for operators, constructors and setters.
; Setters are left out, since the getter already names the property.
(define (describe m)
  (define n (clr-method-name m))
  (cond
    [(regexp-match? #rx"^op_|^\\.|^set_" n) #f]
    [(and (regexp-match? #rx"^get_" n) (null? (clr-method-params m)))
     (hasheq 'name (substring n 4) 'kind "property" 'type (type-text (clr-method-ret m)))]
    [else
     (hasheq 'name n 'kind "method"
             'signature (format "(~a) -> ~a"
                                (string-join (map type-text (clr-method-params m)) ", ")
                                (type-text (clr-method-ret m))))]))

; The public methods of t and of its base classes, most derived first. A method hides an inherited
; one with the same name and parameter types. An interface has no base class, so Object's methods
; are added for it, since every interface value is an object.
(define (inherited-methods t)
  (define chain
    (let loop ([c (class-of t)])
      (define b (base-type c))
      (if b (cons c (loop b)) (list c))))
  (define with-object
    (if (member (class-of 'object) chain) chain (append chain (list (class-of 'object)))))
  (define seen (make-hash))
  (for*/list ([c (in-list with-object)]
              [m (in-list (type-methods c))]
              #:unless (hash-ref seen (list (clr-method-name m) (clr-method-params m)) #f)
              #:do [(hash-set! seen (list (clr-method-name m) (clr-method-params m)) #t)])
    m))

; The Tui type of a numeric literal receiver such as 1 or 1.5, typed as the compiler types a literal,
; or #f when the receiver is not a numeric literal.
(define (literal-type receiver)
  (define n (and (regexp-match? #rx"^[0-9]+(\\.[0-9]+)?$" receiver) (string->number receiver)))
  (cond
    [(not n) #f]
    [(exact-integer? n) (if (<= (- (expt 2 31)) n (sub1 (expt 2 31))) "int32" "int64")]
    [else "float64"]))

(define (member-entries methods static?)
  (for*/list ([m (in-list methods)]
              #:when (eq? (and (clr-method-static? m) #t) static?)
              [e (in-value (describe m))]
              #:when e)
    e))

(define (handle-request req)
  (define id (hash-ref req 'id 'null))
  (define op (hash-ref req 'op #f))
  (define file (hash-ref req 'file #f))
  (define (ok . fields) (apply hasheq 'id id 'ok #t fields))
  (define (fail msg . fields) (apply hasheq 'id id 'ok #f 'error msg fields))
  (with-handlers ([exn:fail? (λ (e) (fail (exn-message e)))])
    (case op
      [("scopes")
       (with-handlers ([exn:fail?
                        (λ (e) (fail (exn-message e)
                                     'kept (and (hash-ref snapshots file #f) #t)))])
         (expand-source file (hash-ref req 'text))
         (ok 'count (length (snapshot-recs (hash-ref snapshots file)))))]
      [("names")
       (define snap (snapshot-for file))
       (ok 'names
           (for/list ([r (in-list (visible snap (hash-ref req 'offset)))])
             (hasheq 'name (rec-name r) 'type (rec-type r) 'kind (record-kind r))))]
      [("types")
       (define snap (snapshot-for file))
       (ok 'types
           (for*/list ([ref (in-list (snapshot-refs snap))]
                       [t (in-list (reference-types ref))])
             (hasheq 'name (car t) 'arity (cadr t) 'namespace (caddr t)
                     'opened (and (member (caddr t) (snapshot-opens snap)) #t))))]
      [("members")
       (define snap (snapshot-for file))
       (define receiver (hash-ref req 'receiver))
       (define value
         (findf (λ (r) (equal? (rec-name r) receiver))
                (visible snap (hash-ref req 'offset))))
       (configure! snap)
       (define literal (and (not value) (literal-type receiver)))
       (define t
         (with-handlers ([exn:fail? (λ (_) #f)])
           (string->tui-type (cond [value (rec-type value)] [literal literal] [else receiver]))))
       (ok 'members
           (member-entries (if t (inherited-methods t) '()) (not (or value literal))))]
      [else (fail (format "unknown op ~a" op))])))

(define (main)
  (let loop ()
    (define line (read-line (current-input-port) 'any))
    (unless (eof-object? line)
      (unless (string=? (string-trim line) "")
        (define req (with-handlers ([exn:fail? (λ (_) #f)]) (string->jsexpr line)))
        (define reply
          (if (hash? req)
              (parameterize ([current-output-port (current-error-port)])
                (handle-request req))
              (hasheq 'ok #f 'error "a request must be a JSON object")))
        (write-json reply)
        (newline)
        (flush-output))
      (loop))))

(module+ main
  (main))
