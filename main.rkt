#lang racket/base
; Tui: a tiny statically typed .NET language. The macro expander is the compiler. Each Tui form is
; a macro that type checks its subforms by expanding them (local-expand stops at the internal
; tui-il form) and produces a typed fragment of Rangi ilasm. Variables are real Racket bindings in
; internal definition contexts, so hygiene and define-syntax-rule macros work unchanged. Calls to
; .NET are resolved at expansion time by reading the referenced assemblies with Rangi's reader.
; The module's program is kept as ilasm datums, assembled with Rangi when it runs.
(require (for-syntax racket/base racket/file racket/list racket/string syntax/parse syntax/parse/define
                     "private/clr.rkt" "private/type-patterns.rkt"
                     "private/type-syntax-class.rkt")
         syntax/parse/define
         (only-in syntax/parse define-syntax-class define-splicing-syntax-class pattern)
         "private/runtime.rkt"
         "private/type-syntax-class.rkt")

(provide (rename-out [tui-module-begin #%module-begin]
                     [tui-top-interaction #%top-interaction]
                     [tui-app #%app] [tui-datum #%datum] [tui-top #%top]
                     [tui-define define] [tui-let let] [tui-set! set!] [tui-if if]
                     [tui-record record]
                     [tui-while while] [tui-begin begin] [tui-and and] [tui-or or] [tui-not not]
                     [tui-try try] [tui-throw throw] [tui-rethrow rethrow]
                     [tui-new new] [tui-list-of list-of] [tui-fn fn]
                     [tui-array array] [tui-new-array new-array] [tui-aref aref]
                     [tui-for-each for-each]
                     [tui-+ +] [tui-- -] [tui-* *] [tui-/ /] [tui-% %]
                     [tui-< <] [tui-> >] [tui-<= <=] [tui->= >=] [tui-= =]
                     [tui-null null]                      [tui-null-of null-of] [tui-null? null?] [tui-typeof typeof])
         ; syntax-rules compiles its patterns at phase 1, so the ellipsis is needed there too
         using open define-syntax-rule define-syntax define-syntax-parser require provide
         define-simple-macro define-syntax-class define-splicing-syntax-class pattern
         begin-for-syntax define-for-syntax define-syntaxes
         define-type-syntax-class type-match type-matches?
         for-syntax file
         ... _ (for-syntax ... _)
         (for-syntax tui-type-of tui-member-return-type tui-type->string class-of
                     tui-resolve-type tui-has-default-ctor?
                     tui-static-overloads tui-instance-overloads tui-member-names
                     supertypes
                     typedExpr typeName
                     type-match type-matches?))

(module reader syntax/module-reader tui)

;; ---------------------------------------------------------------------------------------------
;; The typed IL fragment. Every Tui expression expands to (tui-il (type instructions)).

(define-syntax (tui-il stx)
  (raise-syntax-error 'tui "internal IL fragment used outside a Tui expression" stx))

; When a module body is a single form, the expander partially expands it in a module-begin
; context to check whether it is already a #%module-begin. Every Tui form answers that check by
; wrapping itself in Tui's own module-begin, which then compiles it as usual.
(define-for-syntax ((module-body-form transformer) stx)
  (if (eq? (syntax-local-context) 'module-begin)
      (quasisyntax/loc stx (tui-module-begin #,stx))
      (transformer stx)))

(define-syntax-rule (define-tui-form (name stx) body ...)
  (define-syntax name (module-body-form (λ (stx) body ...))))

; The 'tui-stmt mark says that a form is entered with an empty evaluation stack, which is where a
; try may appear (see tui-try). A transparent form passes the mark to the sub-forms it evaluates
; directly, and the form is returned unchanged otherwise.
(begin-for-syntax
  (define (inherit-stmt from to)
    (if (syntax-property from 'tui-stmt) (syntax-property to 'tui-stmt #t) to)))

; These forms are handled by tui-module-begin and tui-top-interaction.
(define-for-syntax (top-level-only what)
  (module-body-form
   (λ (stx)
     (raise-syntax-error 'tui (format "~a is only allowed at module level or in the REPL" what)
                         stx))))

(define-syntax tui-define (top-level-only "define"))
(define-syntax using (top-level-only "using"))
(define-syntax open (top-level-only "open"))
(define-tui-form (tui-record stx)
  (raise-syntax-error 'tui "record is only allowed at module level" stx))

(begin-for-syntax
  ; Per-method state: local variable types (in slot order) and a label counter. parent is the
  ; enclosing method of a lambda. env lists the (variable . local) pairs that a lambda reads from
  ; its class fields.
  ; names is parallel to locals: the source name of each local, or #f for a compiler temporary.
  (struct mctx ([locals #:mutable] [names #:mutable] [labels #:mutable] parent [env #:mutable]))
  (struct cell-of (type))
  (define current-mctx (make-parameter #f))

  ; Variables that a lambda reads live in one-element arrays (cells), so the lambda and its
  ; enclosing method share them. Each top-level method is compiled in two passes (run-passes).
  ; The discovery pass compiles with plain slots and records in used the ordinal of every variable
  ; that a lambda reads. An ordinal numbers a variable by the order its binding is created, which
  ; is the same in both passes. If the body has a lambda, the second pass makes those variables
  ; cells (cells holds the ordinals from the discovery pass). Lambdas are only lifted in the
  ; second pass.
  (struct pass (discovering? cells used [next #:mutable] [lambdas #:mutable]))
  (define current-pass (make-parameter #f))

  (define (next-ordinal!)
    (define p (current-pass))
    (begin0 (pass-next p) (set-pass-next! p (add1 (pass-next p)))))

  (define (cell-ordinal? ord) (hash-ref (pass-cells (current-pass)) ord #f))

  ; Calls thunk, which compiles one method body and returns its results (for example the code and
  ; the mctx). Returns the results of the second pass if the body has lambdas, otherwise those of the first.
  (define (run-passes thunk)
    (define d (pass #t (make-hasheqv) (make-hasheqv) 0 0))
    (define results (call-with-values (λ () (parameterize ([current-pass d]) (thunk))) list))
    (cond
      [(zero? (pass-lambdas d)) (apply values results)]
      [else
       (call-with-values
        (λ () (parameterize ([current-pass (pass #f (pass-used d) (make-hasheqv) 0 0)])
                (thunk)))
        values)]))

  ; Each lambda becomes a generated class. Its datum is lifted with syntax-local-lift-expression,
  ; and the lifted identifiers wait here until tui-lifts-into adds them to the program.
  (define lifted-classes (box '()))

  (define (the-mctx stx)
    (or (current-mctx) (raise-syntax-error 'tui "Tui expression outside a method body" stx)))

  (define (alloc-local! stx ty [name #f]) (alloc-local-in! (the-mctx stx) ty name))

  (define (new-cell-code ty) (list '(ldc.i4.1) `(newarr ,(tui-type->il ty))))

  ; Rangi requires local names to be unique within a method, but Tui allows shadowing, so a repeated
  ; name gets a numeric suffix.
  (define (unique-local-name m name)
    (and name
         (let loop ([k 1] [cand name])
           (if (member cand (mctx-names m))
               (loop (add1 k) (string->symbol (format "~a_~a" name k)))
               cand))))

  (define (alloc-local-in! m ty [name #f])
    (define i (length (mctx-locals m)))
    (set-mctx-locals! m (append (mctx-locals m) (list ty)))
    (set-mctx-names! m (append (mctx-names m) (list (unique-local-name m name))))
    i)

  (define (new-mctx parent) (mctx '() '() 0 parent '()))

  (define (fresh-label! stx base)
    (define m (the-mctx stx))
    (set-mctx-labels! m (add1 (mctx-labels m)))
    (string->symbol (format "~a~a" base (mctx-labels m))))

  ; The type is also stamped on the form as the 'tui-type syntax property, so it can be read
  ; from an expansion without unpacking the payload.
  (define (il ty instrs)
    (syntax-property #`(tui-il #,(datum->syntax #f (list ty instrs))) 'tui-type ty))

  ; Hover tooltips. Each user expression's type is recorded under its source file, keyed by its
  ; span, so that a required module does not disturb the module being expanded. The module's end
  ; (tui-tooltips) turns the table into 'mouse-over-tooltips for Check Syntax and racket-langserver.
  (define-logger online-check-syntax)
  (define tooltip-tables (make-hash))
  (define tooltip-log-message "Tui tooltip syntaxes; this message is ignored")

  ; racket-langserver shows this text as Markdown, where a bare <int32> is an HTML tag and is
  ; hidden, so the angle brackets of generic types are escaped.
  (define (tooltip-markdown s)
    (regexp-replace* #rx"[<>]" s (λ (m) (string-append "\\" m))))

  (define (record-tooltip! stx ty)
    (define src (syntax-source stx))
    (when (and src (syntax-position stx) (> (syntax-span stx) 0))
      (define tbl (hash-ref! tooltip-tables src make-hash))
      (define key (cons (syntax-position stx) (syntax-span stx)))
      (unless (hash-ref tbl key #f)
        (hash-set! tbl key (cons stx (tooltip-markdown (tui-type->string ty)))))))

  ; Tooltip leaves for one expression, as Typed Racket places them: a compound expression is
  ; marked on its parens only, so overlapping expressions do not clash. Positions are 0-based.
  (define (tooltip-leaves stx text)
    (define start (sub1 (syntax-position stx)))
    (define span (syntax-span stx))
    (if (pair? (syntax-e stx))
        (list (vector stx start (add1 start) text)
              (vector stx (+ start span -1) (+ start span) text))
        (list (vector stx start (+ start span) text))))

  (define (tooltip-vectors tbl)
    (for*/list ([entry (in-list (sort (hash-values tbl) < #:key (λ (e) (syntax-position (car e)))))]
                [leaf (in-list (tooltip-leaves (car entry) (cdr entry)))])
      leaf))

  ; Scopes for completion. Each binder (a parameter, a let name, or a module-level define or value)
  ; is recorded with its type and the span where it is visible, and the module's end logs them on
  ; the tui-scopes topic for the completion helper. Positions are 1-based, as in syntax. A module
  ; binder is visible everywhere, so its span is 0 to +inf.0.
  (define-logger tui-scopes)
  (define scope-records (make-hash))

  (define (record-scope! id text start end)
    (define src (syntax-source id))
    (when (and src start (syntax-position id) (> (syntax-span id) 0))
      (define tbl (hash-ref! scope-records src make-hash))
      (unless (hash-ref tbl (syntax-position id) #f)
        (hash-set! tbl (syntax-position id)
                   (vector (symbol->string (syntax-e id)) text
                           (syntax-position id) (syntax-span id) start end)))))

  ; The span of a list of forms, as (values start end), or (values #f #f) when none has a position.
  (define (forms-span forms)
    (define ps (filter (λ (f) (and (syntax-position f) (> (syntax-span f) 0))) forms))
    (if (null? ps)
        (values #f #f)
        (values (apply min (map syntax-position ps))
                (apply max (map (λ (f) (+ (syntax-position f) (syntax-span f))) ps)))))

  ; Debug line markers. A statement of a compiled module-level form gets a .line directive for its
  ; source span, so the PDB maps its code to the source. A statement from a macro template keeps the
  ; template's position, which is outside the module's compiled forms (the template is in a macro
  ; definition, or in another file), so it gets no marker. For each source file, the line starts
  ; and the 1-based spans of the compiled forms are recorded when the module is expanded.
  (struct line-source (starts spans))
  (define line-sources (make-hash))

  (define (record-line-source! src forms)
    (hash-remove! line-sources src)
    ; The reader's positions count a \r\n pair as one character, so the text is normalized the same way.
    (define text (and (path? src)
                      (with-handlers ([exn:fail:filesystem? (λ (e) #f)])
                        (regexp-replace* #rx"\r\n" (file->string src) "\n"))))
    (when text
      (define spans
        (for/list ([f (in-list forms)] #:when (and (syntax-position f) (> (syntax-span f) 0)))
          (cons (syntax-position f) (+ (syntax-position f) (syntax-span f)))))
      (hash-set! line-sources src (line-source (line-starts text) spans))))

  ; The 0-based offset of the first character of each line. A line break is \n, \r\n, or a lone \r.
  (define (line-starts text)
    (define n (string-length text))
    (list->vector
     (cons 0
           (for/list ([i (in-range n)]
                      #:when (let ([c (string-ref text i)])
                               (or (char=? c #\newline)
                                   (and (char=? c #\return)
                                        (not (and (< (add1 i) n)
                                                  (char=? (string-ref text (add1 i)) #\newline)))))))
             (add1 i)))))

  ; The 1-based line and 0-based column of a 0-based offset.
  (define (offset-line+column starts o)
    (let search ([lo 0] [hi (sub1 (vector-length starts))])
      (cond
        [(= lo hi) (values (add1 lo) (- o (vector-ref starts lo)))]
        [else
         (define mid (quotient (+ lo hi 1) 2))
         (if (<= (vector-ref starts mid) o)
             (search mid hi)
             (search lo (sub1 mid)))])))

  ; The .line directive for the source span of stx, as a list of zero or one instruction. The list is
  ; empty unless stx lies in a compiled form of its module, or its columns do not fit in Rangi.
  (define (line-marker stx)
    (define src (syntax-source stx))
    (define ls (and (path? src) (hash-ref line-sources src #f)))
    (define p (syntax-position stx))
    (cond
      [(and ls p (> (syntax-span stx) 0)
            (for/or ([sp (in-list (line-source-spans ls))]) (and (<= (car sp) p) (< p (cdr sp)))))
       (define starts (line-source-starts ls))
       (define-values (l1 c1) (offset-line+column starts (sub1 p)))
       ; the end is the column after the last character
       (define-values (l2 c2) (offset-line+column starts (+ p (syntax-span stx) -2)))
       (if (and (<= c1 #xFFFF) (<= (add1 c2) #xFFFF))
           (list `(.line ,l1 ,c1 ,l2 ,(add1 c2) ,(path->string src)))
           '())]
      [else '()]))

  ; Expands stx to a typed fragment: (values type instructions). origin is the source syntax
  ; that the type is recorded for, when stx is already expanded and has lost its location.
  (define (tc stx [ctx #f] [origin stx])
    (define e (local-expand stx 'expression (list #'tui-il) ctx))
    (syntax-case e ()
      [(head payload)
       (and (identifier? #'head) (free-identifier=? #'head #'tui-il))
       (let* ([p (syntax->datum #'payload)]
              [ty (or (syntax-property e 'tui-type) (error 'tui "internal: untyped fragment"))])
         (record-tooltip! origin ty)
         (values ty (cadr p)))]
      [_ (raise-syntax-error 'tui "not a Tui expression" stx)]))

  ; Expands stx and converts it to type `to`, or raises a type error at stx. With #:assign? the
  ; value cannot be never, since a variable or element would never receive it.
  (define (tc/expect stx to [origin stx] #:assign? [assign? #f])
    (define-values (ty code) (tc (syntax-property stx 'tui-expected to) #f origin))
    (when (and assign? (eq? ty 'never))
      (raise-syntax-error 'tui "cannot assign a value that never completes" stx))
    (define c (coercion ty to))
    (unless c
      (raise-syntax-error 'tui (format "type mismatch: expected ~a, got ~a"
                                       (tui-type->string to) (tui-type->string ty))
                          stx))
    (append code (cdr c)))

  (define (tc* stxs)
    (for/lists (tys codes) ([s (in-list stxs)]) (tc s)))

  ; Variables. kind is 'arg or 'local; the binding's value is the transformer itself, so
  ; set! can find the slot with syntax-local-value. owner is the method the slot belongs to, so
  ; a lambda body (its own method) cannot reach the enclosing method's slots.
  ; kind is 'arg, 'local (a plain slot of owner's method) or 'cell (index is the local of owner
  ; that holds the cell array; the variable's value is element 0).
  ; (f arg ...) on a delegate value f is the same call as (.Invoke f arg ...). The rewritten form
  ; is expanded again, so the argument checks and the Invoke call are those of .Invoke.
  (define (delegate-call stx ty)
    (syntax-parse stx
      [(f:id arg ...)
       (unless (delegate-type? ty)
         (raise-syntax-error 'tui (format "~a is ~a, not a delegate, so it cannot be called"
                                          (syntax-e #'f) (tui-type->string ty))
                             stx))
       (quasisyntax/loc stx (tui-app .Invoke f arg ...))]
      [_ (raise-syntax-error 'tui "a delegate call needs a name, as in (f arg ...)" stx)]))

  (struct tui-var (kind index type owner ord)
    #:property prop:procedure
    (λ (self stx)
      (cond
        [(identifier? stx) (il (tui-var-type self) (load-var self stx))]
        [else (delegate-call stx (tui-var-type self))])))

  (define (make-tui-var kind index type ord) (tui-var kind index type (current-mctx) ord))

  ; The slot of v in m. In the discovery pass a lambda may read v through a plain slot; the read
  ; is recorded and the code is thrown away, so index 0 is returned. The second pass has made v a
  ; cell in that case, so a plain slot from another method cannot happen there.
  (define (plain-slot v m stx)
    (cond
      [(eq? (tui-var-owner v) m) (tui-var-index v)]
      [(pass-discovering? (current-pass))
       (hash-set! (pass-used (current-pass)) (tui-var-ord v) #t)
       0]
      [else (error 'tui "internal: ~a is read from a lambda but was not made a cell" (syntax-e stx))]))

  ; The field of a lambda's class that holds its k-th env cell, and that field's type.
  (define (env-field k) (string->symbol (format "cap~a" k)))
  (define (env-field-type v) `(szarray ,(tui-type->il (tui-var-type v))))

  ; The local of m that holds v's cell. A lambda reads the cells it uses from its class fields,
  ; and a lambda inside a lambda gets them from its parent, so each level adds what it needs.
  (define (cell-slot-in m v)
    (cond
      [(eq? (tui-var-owner v) m) (tui-var-index v)]
      [(assq v (mctx-env m)) => cdr]
      [else
       (let ([p (or (mctx-parent m) (error 'tui "internal: a cell has no enclosing method"))])
         (cell-slot-in p v)
         (let ([j (alloc-local-in! m (cell-of (tui-var-type v)))])
           (set-mctx-env! m (append (mctx-env m) (list (cons v j))))
           j))]))

  (define (load-var v stx)
    (define m (the-mctx stx))
    (case (tui-var-kind v)
      [(cell) (append (list (var-instr 'ld 'local (cell-slot-in m v)))
                      '((ldc.i4.0))
                      `((ldelem ,(tui-type->il (tui-var-type v)))))]
      [else (list (var-instr 'ld (tui-var-kind v) (plain-slot v m stx)))]))

  ; Code that stores the value pushed by value-code into v.
  (define (store-var v stx value-code)
    (define m (the-mctx stx))
    (case (tui-var-kind v)
      [(cell) (append (list (var-instr 'ld 'local (cell-slot-in m v)))
                      '((ldc.i4.0))
                      value-code
                      `((stelem ,(tui-type->il (tui-var-type v)))))]
      [else (append value-code (list (var-instr 'st (tui-var-kind v) (plain-slot v m stx))))]))

  ; III.3.38 ldarg, III.3.43 ldloc, III.3.61 starg, III.3.63 stloc, short forms where they fit.
  (define (var-instr op kind i)
    (define base (format "~a~a" op (if (eq? kind 'arg) "arg" "loc")))
    (cond
      [(and (< i 4) (not (and (eq? op 'st) (eq? kind 'arg))))
       (list (string->symbol (format "~a.~a" base i)))]
      [(< i 256) (list (string->symbol (string-append base ".s")) i)]
      [else (list (string->symbol base) i)]))

  ; Binds each (id kind index type ordinal) in a fresh definition context, then type checks body there.
  (define (tc/bind binds body)
    (define ctx (syntax-local-make-definition-context))
    (for ([b (in-list binds)])
      (syntax-local-bind-syntaxes
       (list (internal-definition-context-introduce ctx (car b) 'add))
       #`(make-tui-var '#,(cadr b) #,(caddr b) '#,(cadddr b) '#,(list-ref b 4))
       ctx))
    (tc (internal-definition-context-introduce ctx body 'add) ctx))

  ; A type annotation (an identifier) to a Tui type. A quoted annotation such as |int32[]| or
  ; |List<int32>| is parsed from its text.
  (define (ann->type stx)
    (define text (and (identifier? stx) (symbol->string (syntax-e stx))))
    (or (if (and text (regexp-match? #rx"[][<>]" text))
            (with-clr stx (λ () (string->tui-type text)))
            (and (identifier? stx) (resolve-type-name stx)))
        (raise-syntax-error 'tui "unknown type" stx)))

  ; Errors from the CLR layer become syntax errors at stx.
  (define (with-clr stx thunk)
    (with-handlers ([(λ (e) (and (exn:fail? e) (not (exn:fail:syntax? e))))
                     (λ (e) (raise-syntax-error
                             'tui (regexp-replace #rx"^tui: " (exn-message e) "") stx))])
      (thunk)))

  ; Argument code followed by each argument's conversion, in order.
  (define (args-code codes convs)
    (append* (map append codes convs)))

  ; The arguments of a CLR call are typed before the overload is chosen, except bare fns. A bare
  ; fn (one without a delegate name) is typed as a placeholder of its arity and compiled against
  ; the chosen overload's parameter type by finish-call. Each argument gives its type and its code,
  ; or a deferred-fn.
  (struct deferred-fn (stx origin))

  ; The arity of a bare fn form (fn (p ...) body ...+) as expanded, or #f for any other form.
  (define (bare-fn-arity e)
    (define xs (syntax->list e))
    (and xs
         (>= (length xs) 3)
         (identifier? (car xs))
         (free-identifier=? (car xs) #'tui-fn 0 0)
         (let ([ps (syntax->list (cadr xs))])
           (and ps (length ps)))))

  ; A bare null is the identifier null, which has no type until an expected type is known.
  (define (bare-null? s)
    (and (identifier? s) (free-identifier=? s #'tui-null 0 0)))

  (define (tc-args stxs)
    (for/lists (tys codes) ([s (in-list stxs)])
      (cond
        [(bare-null? s) (values null-placeholder (deferred-fn s s))]
        [else
         (define e (local-expand s 'expression (list #'tui-il #'tui-fn) #f))
         (define n (bare-fn-arity e))
         (if n
             (values (lambda-placeholder n) (deferred-fn e s))
             (tc e))])))

  ; The code of each argument for the closed method's parameter types params, with the conversions
  ; from resolve-overload. A deferred fn is compiled against its parameter type, and needs no
  ; conversion.
  (define (finish-args codes convs params)
    (append*
     (for/list ([c (in-list codes)] [conv (in-list convs)] [p (in-list params)])
       (if (deferred-fn? c)
           (tc/expect (deferred-fn-stx c) p (deferred-fn-origin c))
           (append c conv)))))

  ; Compiles a bare fn whose parameter type p may still have open type arguments, held in the
  ; vector slots (filled in here). The fn's parameters take the types that p already fixes, and the
  ; body's Func or Action type then fixes the rest through bind-type. Returns the fn's type and code.
  (define (compile-partial-fn e p slots)
    (define known (subst-mvars p (vector->list slots)))
    (define xs (syntax->list e))
    (define ptys
      (for/list ([i (in-list (delegate-param-args known))])
        (define t (and i (list-ref (class-args known) i)))
        (unless (and t (not (has-mvar? t)))
          (raise-syntax-error 'tui "cannot infer the type of a parameter of this fn" e))
        t))
    (define typed
      (for/list ([a (in-list (lambda-params (syntax->list (cadr xs))))] [t (in-list ptys)])
        (when (and (cdr a) (not (equal? (cdr a) t)))
          (raise-syntax-error 'tui (format "parameter ~a is ~a but the delegate passes ~a"
                                           (syntax-e (car a)) (tui-type->string (cdr a))
                                           (tui-type->string t))
                              e (car a)))
        (cons (car a) t)))
    (define-values (fty fcode) (tc (compile-lambda e #f typed (cddr xs))))
    (unless (bind-type p fty slots (make-vector (vector-length slots) #f))
      (raise-syntax-error 'tui (format "fn has type ~a, which does not fit ~a"
                                       (tui-type->string fty) (tui-type->string p))
                          e))
    (values fty fcode))

  ; The closed method and the argument code for a call to m, which resolve-overload chose. tys are
  ; the argument types (a placeholder for a bare fn) and codes their codes (a deferred-fn for a bare
  ; fn). Most calls are closed already and use the conversions from resolve-overload. When some
  ; type argument is still open (see infer-targs), the bare fns fix it, and the arguments are then
  ; converted to the closed parameter types.
  (define (finish-call stx tys codes convs m)
    (define targs (clr-method-targs m))
    (cond
      [(not (memv #f targs))
       (values m (finish-args codes convs (clr-method-params m)))]
      [else
       (define slots (list->vector targs))
       (define compiled
         (for/list ([c (in-list codes)] [t (in-list tys)] [p (in-list (clr-method-params m))])
           (cond
             ; a null fixes no type argument, so it is compiled once the method is closed
             [(and (deferred-fn? c) (bare-null? (deferred-fn-stx c))) (list #f c)]
             [(deferred-fn? c)
              (let-values ([(fty fcode) (compile-partial-fn (deferred-fn-stx c) p slots)])
                (record-tooltip! (deferred-fn-origin c) fty)
                (list fty fcode))]
             [else (list t c)])))
       (define open
         (for/list ([s (in-vector slots)] [i (in-naturals)] #:unless s) i))
       (unless (null? open)
         (raise-syntax-error 'tui (format "cannot infer the type arguments of ~a"
                                          (clr-method-name m))
                             stx))
       (define m* (instantiate-method m (vector->list slots)))
       (values
        m*
        (append*
         (for/list ([cl (in-list compiled)] [p (in-list (clr-method-params m*))])
           (cond
             [(not (car cl))
              (define nul (cadr cl))
              (tc/expect (deferred-fn-stx nul) p (deferred-fn-origin nul))]
             [else
              (define c (coercion (car cl) p))
              (unless c
                (raise-syntax-error 'tui (format "argument has type ~a, expected ~a"
                                                 (tui-type->string (car cl)) (tui-type->string p))
                                    stx))
              (append (cadr cl) (cdr c))]))))]))

  ;; Tui functions: (define (f [x : T] ...) [: R] body ...+). The binding of f holds its declared
  ;; signature as syntax; types are resolved when used, after the module's references are set.
  ; ret is #f when the return type is omitted. Then it is the type of the body, inferred from def
  ; when first needed and kept in inferred.
  (struct tui-fun (name params ret def [inferred #:mutable])
    #:property prop:procedure
    (λ (self stx)
      (syntax-parse stx
        [(f:id arg ...) (compile-tui-call self stx (syntax->list #'(arg ...)))]
        [_ (raise-syntax-error 'tui "functions are not first-class in Tui; call it instead" stx)])))

  (define (make-tui-fun name params ret def) (tui-fun name params ret def #f))

  ; The type of a definition whose type is omitted, inferred from its body the first time it is
  ; needed. get and put! read and write the cached type, and infer computes it. While it is being
  ; inferred the cache holds 'pending, so a definition that needs its own type is recursive.
  (define (infer-once! get put! name stx what infer)
    (define known (get))
    (cond
      [(eq? known 'pending)
       (raise-syntax-error 'tui (format "cannot infer the ~a of ~a, which depends on itself; add a ~a annotation"
                                        what name what)
                           stx)]
      [known known]
      [else
       (put! 'pending)
       (define t (with-handlers ([exn:fail? (λ (e) (put! #f) (raise e))]) (infer)))
       (put! t)
       t]))

  ; The return type of f, called from stx.
  (define (tui-fun-result-type f stx)
    (cond
      [(tui-fun-ret f) => ann->type]
      [else
       (infer-once! (λ () (tui-fun-inferred f)) (λ (t) (set-tui-fun-inferred! f t))
                    (tui-fun-name f) stx "return type"
                    (λ () (infer-function-type (tui-fun-def f))))]))

  ; Module values: (define name : T expr). The binding of name holds its declared type as syntax.
  ; A reference loads the static field of <Module>, which is set when the program starts. While a
  ; value's initializer is compiled, value-scope lists the values that are already set, and a
  ; reference to any other module value is an error.
  (define value-scope (make-parameter #f))

  ; A module value (define name [: T] expr). type is the annotation as syntax, or #f when it is
  ; omitted, and then the type is inferred from def the first time it is needed. earlier lists the
  ; values set before this one, as symbols.
  (struct tui-global (name type def earlier [inferred #:mutable])
    #:property prop:procedure
    (λ (self stx)
      (cond
        [(identifier? stx)
         (check-global-scope self stx)
         (let ([t (global-result-type self stx)])
           (il t (list `(ldsfld Program ,(tui-global-name self) ,(tui-type->il t)))))]
        [else (delegate-call stx (global-result-type self stx))])))

  (define (make-tui-global name type def earlier) (tui-global name type def earlier #f))

  (define (global-result-type g stx)
    (cond
      [(tui-global-type g) => ann->type]
      [else
       (infer-once! (λ () (tui-global-inferred g)) (λ (t) (set-tui-global-inferred! g t))
                    (tui-global-name g) stx "type"
                    (λ () (infer-value-type (tui-global-def g) (tui-global-earlier g))))]))

  ; The type of the expression of a module value def, found by a discovery pass in the scope of
  ; the values set before it.
  (define (infer-value-type def earlier)
    (define-values (_code _m bty)
      (parameterize ([current-pass (pass #t (make-hasheqv) (make-hasheqv) 0 0)]
                     [value-scope earlier])
        (compile-body def '() (list (last (syntax->list def))) #f #f #f)))
    (when (eq? bty 'void)
      (raise-syntax-error 'tui "a module value cannot be void" def))
    bty)

  (define (check-global-scope g stx)
    (define scope (value-scope))
    (when (and scope (not (memq (tui-global-name g) scope)))
      (raise-syntax-error 'tui "module value used before it is defined" stx)))

  (define (compile-tui-call f stx args)
    (define ptys (map ann->type (tui-fun-params f)))
    (unless (= (length args) (length ptys))
      (raise-syntax-error 'tui (format "~a expects ~a arguments, got ~a"
                                       (tui-fun-name f) (length ptys) (length args))
                          stx))
    (define rty (tui-fun-result-type f stx))
    (il rty (append (append* (map tc/expect args ptys))
                    `((call ,(tui-type->il rty) (Program) ,(tui-fun-name f)
                            ,(map tui-type->il ptys))))))

  (define-syntax-class param
    (pattern [name:id (~datum :) type:id]))

  ; The return type is #f when it is omitted, and then it is inferred from the body.
  (define (fun-header def)
    (syntax-parse def
      [(_ (name:id p:param ...) (~datum :) ret:id body ...+)
       (values #'name (syntax->list #'(p.name ...)) (syntax->list #'(p.type ...)) #'ret
               (syntax->list #'(body ...)))]
      [(_ (name:id p:param ...) body ...+)
       (values #'name (syntax->list #'(p.name ...)) (syntax->list #'(p.type ...)) #f
               (syntax->list #'(body ...)))]
      [_ (raise-syntax-error 'tui "expected (define (name [param : type] ...) [: type] body ...+)"
                             def)]))

  ; The type of the body of function def, found by a discovery pass that emits no code.
  (define (infer-function-type def)
    (define-values (name pnames ptypes ret body) (fun-header def))
    (define ptys (map ann->type ptypes))
    (define-values (_code _m bty)
      (parameterize ([current-pass (pass #t (make-hasheqv) (make-hasheqv) 0 0)]
                     [value-scope #f])
        (compile-body def (for/list ([p pnames] [t ptys] [i (in-naturals)]) (list p 'arg i t))
                      body #f #f #f)))
    bty)

  ; Compiles one method body. binds are (id kind index type), parent is the enclosing method of
  ; a lambda (#f for a method), and owner is the lambda's class (#f for a method). rty is the
  ; return type, or #f to return the body's type. Returns the instructions, with the return
  ; conversion and ret, the mctx that holds the locals and the lambda's env, and the return type.
  (define (compile-body stx binds body rty parent owner)
    (define m (new-mctx parent))
    (parameterize ([current-mctx m] [in-catch-handler #f])
      (define-values (body-start body-end) (forms-span body))
      ; A captured argument is copied into a cell, so the lambda and its enclosing method share it.
      (define-values (cell-binds arg-code)
        (for/fold ([bs '()] [code '()] #:result (values (reverse bs) (append* (reverse code))))
                  ([b (in-list binds)])
          (define id (car b))
          (define kind (cadr b))
          (define index (caddr b))
          (define ty (cadddr b))
          (define ord (next-ordinal!))
          (record-tooltip! id ty)
          (record-scope! id (tui-type->string ty) body-start body-end)
          (cond
            [(and (eq? kind 'arg) (cell-ordinal? ord))
             (define j (alloc-local-in! m (cell-of ty) (syntax-e id)))
             (values (cons (list id 'cell j ty ord) bs)
                     (cons (append (new-cell-code ty)
                                   (list (var-instr 'st 'local j)
                                         (var-instr 'ld 'local j)
                                         '(ldc.i4.0)
                                         (var-instr 'ld 'arg index)
                                         `(stelem ,(tui-type->il ty))))
                           code))]
            [else (values (cons (append b (list ord)) bs) code)])))
      (define n (length body))
      (define tail-body
        (for/list ([b (in-list body)] [i (in-naturals 1)])
          (if (and (= i n) rty (not (eq? rty 'void))) (syntax-property b 'tui-expected rty) b)))
      (define-values (bty code) (tc/bind cell-binds (syntax-property #`(tui-begin #,@tail-body) 'tui-stmt #t)))
      (when (and (not rty) (eq? bty 'never))
        (raise-syntax-error 'tui "cannot infer a result type: the body always throws, so annotate the result type"
                            stx))
      (define ret-code
        (cond
          [(not rty) '()]
          [(eq? bty 'never) '()]
          [(eq? rty 'void) (if (eq? bty 'void) '() '((pop)))]
          [(coercion bty rty) => cdr]
          [else (raise-syntax-error 'tui (format "function returns ~a but its body has type ~a"
                                                 (tui-type->string rty) (tui-type->string bty))
                                    stx)]))
      ; Copies the cells that this lambda reads from its fields into locals (this is argument 0).
      (define env-code
        (append* (for/list ([e (in-list (mctx-env m))] [k (in-naturals)])
                   (list '(ldarg.0)
                         `(ldfld ,owner ,(env-field k) ,(env-field-type (car e)))
                         (var-instr 'st 'local (cdr e))))))
      (values (append env-code arg-code code ret-code '((ret))) m (or rty bty))))

  (define (local-il t)
    (if (cell-of? t) `(szarray ,(tui-type->il (cell-of-type t))) (tui-type->il t)))

  (define (locals-decl m)
    (define entries
      (for/list ([t (in-list (mctx-locals m))] [n (in-list (mctx-names m))])
        (if n `(,(local-il t) ,n) (local-il t))))
    (if (null? entries) '() `((.locals init ,entries))))

  ; Parameter declarations with names, so the debugger can show them. The call signature is
  ; unchanged, so callers still use the plain types.
  (define (params-decl tys names)
    (for/list ([t (in-list tys)] [n (in-list names)])
      `(param () ,(tui-type->il t) ,(syntax-e n))))

  (define (compile-function def)
    (define-values (name pnames ptypes ret body) (fun-header def))
    (define ptys (map ann->type ptypes))
    (define rty (tui-fun-result-type (syntax-local-value name) def))
    (record-scope! name
                   (format "(~a) -> ~a" (string-join (map tui-type->string ptys) ", ")
                           (tui-type->string rty))
                   0 +inf.0)
    (define-values (code m _rty)
      (run-passes
       (λ ()
         (compile-body def (for/list ([p pnames] [t ptys] [i (in-naturals)]) (list p 'arg i t))
                       body rty #f #f))))
      `(.method (public static) ,(tui-type->il rty) ,(syntax-e name) ,(params-decl ptys pnames)
                (cil managed)
                (,@(locals-decl m) ,@code)))

  ; A module value (define name : T expr). The field is a static field of <Module>, and the static
  ; method __init_name computes the value. earlier lists the values that are set before this one.
  (define (value-init-name name) (string->symbol (format "__init_~a" name)))

  (define (value-parts def earlier)
    (syntax-parse def
      [(_ name:id (~optional (~seq (~datum :) t:id)) e)
       (define rty (global-result-type (syntax-local-value #'name) def))
       (record-scope! #'name (tui-type->string rty) 0 +inf.0)
       (define-values (code m _rty)
         (run-passes
          (λ ()
            (parameterize ([value-scope (map syntax-e earlier)])
              (compile-body def '() (list #'e) rty #f #f)))))
       (values `(.method (public static) ,(tui-type->il rty) ,(value-init-name (syntax-e #'name))
                         () (cil managed)
                         (,@(locals-decl m) ,@code))
               `(.field (public static) ,(tui-type->il rty) ,(syntax-e #'name)))]
      [_ (raise-syntax-error 'tui "expected (define name [: type] expr)" def)]))

  ; The static initializers, run in order at the start of the entry point. inits is a list of
  ; (name type) in source order.
  (define (value-inits inits)
    (append*
     (for/list ([i (in-list inits)])
       (define name (car i))
       (define t (tui-type->il (cadr i)))
       `((call ,t (Program) ,(value-init-name name) ())
         (stsfld Program ,name ,t)))))

  ; The parameter and return types of the Invoke method of delegate type t.
  (define (delegate-signature stx t)
    (unless (delegate-type? t)
      (raise-syntax-error 'tui (format "fn needs a delegate type, got ~a" (tui-type->string t))
                          stx))
    (define groups (with-clr stx (λ () (find-instance-methods t "Invoke"))))
    (unless (pair? groups)
      (raise-syntax-error 'tui "delegate has no Invoke method" stx))
    (define m (car (car groups)))
    (values (clr-method-params m) (clr-method-ret m)))

  ; The type that calls in body fix for each parameter in pnames, or #f. A parameter is fixed when
  ; it is a direct argument of a call (Type.Method arg ...) or (.Method recv arg ...) whose name and
  ; arity have exactly one overload, that is not generic and has a closed type at that position.
  ; Nested fn, let, let*, letrec and define are not scanned, since their binders could shadow.
  (define (body-param-types pnames body)
    (define found (make-vector (length pnames) #f))
    (define (param-index id)
      (for/first ([p (in-list pnames)] [i (in-naturals)] #:when (free-identifier=? p id)) i))
    ; The overloads of a call that take n arguments, from the first type in the base chain that has
    ; any, as hiding would leave them.
    (define (arity-overloads groups n)
      (define (of-n g) (filter (λ (m) (= (length (clr-method-params m)) n)) g))
      (define hit (for/first ([g (in-list groups)] #:when (pair? (of-n g))) (of-n g)))
      (or hit '()))
    ; The candidate overloads of a call, and the argument expressions that line up with their
    ; parameters. Any failure to look a call up means there is nothing to learn from it.
    (define (call-candidates head args)
      (define name (symbol->string (syntax-e head)))
      (with-handlers ([exn:fail? (λ (_) (cons '() '()))])
        (cond
          [(regexp-match? #rx"<" name) (cons '() '())]
          [(regexp-match #rx"^\\.([^.]+)$" name)
           => (λ (m)
                (define recv (and (pair? args) (car args)))
                (define v (and recv (identifier? recv) (syntax-local-value recv (λ () #f))))
                (define t (and (tui-var? v) (tui-var-type v)))
                (cond
                  [(not t) (cons '() '())]
                  [else
                   (define owner (if (primitive-value-type? t) 'object t))
                   (define rest (cdr args))
                   (cons (arity-overloads (find-instance-methods owner (cadr m)) (length rest))
                         rest)]))]
          [(and (dotted-match name) (local-receiver head name))
           => (λ (v)
                (define m (dotted-match name))
                (define t (tui-var-type v))
                (define owner (if (primitive-value-type? t) 'object t))
                (cons (arity-overloads (find-instance-methods owner (caddr m)) (length args))
                      args))]
          [(regexp-match #rx"^(.+)\\.([^.]+)$" name)
           => (λ (m)
                (cons (arity-overloads (list (find-static-methods (cadr m) (caddr m)))
                                       (length args))
                      args))]
          [(delegate-value-type head)
           => (λ (t)
                (cons (arity-overloads (find-instance-methods t "Invoke") (length args)) args))]
          [else (cons '() '())])))
    (define (note-call! head args)
      (define c (call-candidates head args))
      (define cands (car c))
      (when (and (= (length cands) 1) (zero? (clr-method-arity (car cands))))
        (for ([a (in-list (cdr c))] [t (in-list (clr-method-params (car cands)))])
          (define i (and (identifier? a) (param-index a)))
          (when (and i (not (vector-ref found i)) (not (has-mvar? t)))
            (vector-set! found i t)))))
    (define (scan e)
      (define xs (syntax->list e))
      (when (and xs (pair? xs))
        (define head (car xs))
        (unless (and (identifier? head) (memq (syntax-e head) '(fn let let* letrec define)))
          (when (identifier? head) (note-call! head (cdr xs)))
          (for-each scan xs))))
    (for-each scan body)
    (vector->list found))

  ; (fn Delegate (param ...) body ...+). The body is the Invoke method of a generated class. The
  ; class has one field per cell that the body reads, and an instance of it is the delegate's
  ; target. dt is the delegate type, or #f when there is none, in which case every parameter is
  ; annotated and the delegate is Func or Action of the body's type. params are (id . type-or-#f).
  (define (compile-lambda stx dt params body)
    (define pnames (map car params))
    (define anns (map cdr params))
    (define-values (dt0 ptys rty)
      (cond
        [dt
         (define-values (ptys rty) (delegate-signature stx dt))
         (unless (= (length pnames) (length ptys))
           (raise-syntax-error 'tui (format "fn has ~a parameters but ~a takes ~a"
                                            (length pnames) (tui-type->string dt) (length ptys))
                               stx))
         (for ([p (in-list pnames)] [a (in-list anns)] [t (in-list ptys)])
           (when (and a (not (equal? a t)))
             (raise-syntax-error 'tui (format "parameter ~a is ~a but ~a takes ~a"
                                              (syntax-e p) (tui-type->string a)
                                              (tui-type->string dt) (tui-type->string t))
                                 stx p)))
         (values dt ptys rty)]
        [else
         (define anns* (for/list ([a (in-list anns)] [f (in-list (body-param-types pnames body))])
                         (or a f)))
         (unless (andmap values anns*)
           (raise-syntax-error 'tui "fn without a delegate type needs an expected delegate type or annotated parameters"
                               stx))
         (values #f anns* #f)]))
    (define parent (current-mctx))
    (define cname (string->symbol (symbol->string (gensym 'closure))))
    (set-pass-lambdas! (current-pass) (add1 (pass-lambdas (current-pass))))
    (define-values (code m body-rty)
      (compile-body stx (for/list ([p pnames] [t ptys] [i (in-naturals 1)]) (list p 'arg i t))
                    body rty parent cname))
    (define dt* (or dt0 (lambda-delegate stx ptys body-rty)))
    (if (pass-discovering? (current-pass))
        (il dt* '((ldnull)))
        (let ()
          (define env (mctx-env m))
          (define ils (map tui-type->il ptys))
          (define ret-il (tui-type->il body-rty))
          (define fields
            (for/list ([e (in-list env)] [k (in-naturals)])
              `(.field (public) ,(env-field-type (car e)) ,(env-field k))))
          (define ctor
            '(.method (public specialname rtspecialname) void .ctor () (cil managed)
                      ((ldarg.0) (call void (mscorlib System.Object) .ctor ()) (ret))))
          (define invoke
            `(.method (public) ,ret-il Invoke ,(params-decl ptys pnames) (cil managed)
                      (,@(locals-decl m) ,@code)))
          (define class-datum `(.class (public auto ansi) ,cname (,@fields ,ctor ,invoke)))
          (set-box! lifted-classes
                    (cons (syntax-local-lift-expression
                           #`(#,(quote-syntax quote) #,(datum->syntax #f class-datum)))
                          (unbox lifted-classes)))
          (define env-build
            (append* (for/list ([e (in-list env)] [k (in-naturals)])
                       (list '(dup)
                             (var-instr 'ld 'local (cell-slot-in parent (car e)))
                             `(stfld ,cname ,(env-field k) ,(env-field-type (car e)))))))
          (il dt* `((newobj (,cname) ())
                    ,@env-build
                    (ldftn ,ret-il (,cname) Invoke ,ils)
                    (newobj ,(owner->il dt*) (object intptr)))))))

  ; The Func or Action type for a lambda with parameter types ptys and return type rty.
  (define (lambda-delegate stx ptys rty)
    (or (if (eq? rty 'void)
            (generic-instance "System.Action" ptys)
            (generic-instance "System.Func" (append ptys (list rty))))
        (raise-syntax-error 'tui (format "fn cannot take ~a parameters" (length ptys)) stx)))

  ; The (id . type-or-#f) pairs of a fn's parameter list, each either id or [id : type].
  (define (lambda-params ps)
    (for/list ([p (in-list ps)])
      (syntax-parse p
        [n:id (cons #'n #f)]
        [[n:id (~datum :) t:id] (cons #'n (ann->type #'t))]
        [_ (raise-syntax-error 'tui "expected a parameter name or [name : type]" p)])))



  ; The entry point. With print? the value of the last form, if any, is written to the console.
  (define (compile-main stx forms print? [inits '()])
    (define-values (code m)
      (run-passes
       (λ ()
         (parameterize ([current-mctx (new-mctx #f)])
           (define-values (ty code) (tc/bind '() (syntax-property #`(tui-begin #,@forms) 'tui-stmt #t)))
           (define tail
             (cond
               [(eq? ty 'void) '()]
               [print? (let-values ([(m convs) (with-clr stx
                                                 (λ () (resolve-overload
                                                        (list (find-static-methods "System.Console"
                                                                                   "WriteLine"))
                                                        (list ty) "Console.WriteLine")))])
                         (append (car convs) (list (clr-call-instr 'call m))))]
               [else '((pop))]))
           (values (append code tail '((ret))) (current-mctx))))))
    `(.method (public static) void __tui_main () (cil managed)
              ((.entrypoint) ,@(locals-decl m) ,@(value-inits inits) ,@code)))

  (define (clr-call-instr op m)
    `(,op ,@(if (clr-method-static? m) '() '(instance))
          ,(clr-method-il-ret m) ,(owner->il (clr-method-owner m))
          ,(string->symbol (clr-method-name m)) ,(clr-method-il-params m)
          ,@(if (null? (clr-method-targs m))
                '()
                `((inst ,@(map tui-type->il (clr-method-targs m)))))))

  ;; Module-level form classification, shared by #%module-begin and #%top-interaction.
  (define (form-kind f)
    (syntax-parse f
      [(h:id . _)
       (cond
         [(free-identifier=? #'h #'tui-define)
          (let ([l (syntax->list f)])
            (if (and (pair? (cdr l)) (identifier? (cadr l))) 'value 'define))]
         [(free-identifier=? #'h #'using) 'using]
         [(free-identifier=? #'h #'open) 'open]
         [(free-identifier=? #'h #'tui-record) 'record]
         [(ormap (λ (k) (free-identifier=? #'h k))
                 (list #'define-syntax-rule #'define-syntax #'define-syntax-parser
                       #'define-simple-macro #'define-syntax-class
                       #'define-splicing-syntax-class
                       #'begin-for-syntax #'define-for-syntax #'define-syntaxes
                       #'define-type-syntax-class
                       #'require #'provide))
          'passthrough]
         [else 'expr])]
      [_ 'expr]))

  (define (using-path f)
    (syntax-parse f
      [(_ p:str) (with-clr f (λ () (tui-add-reference! (syntax-e #'p))
                               (last (tui-references))))]
      [_ (raise-syntax-error 'tui "expected (using \"Assembly\") or (using \"path.dll\")" f)]))

  (define (open-namespace f)
    (syntax-parse f
      [(_ ns:id) (symbol->string (syntax-e #'ns))]
      [_ (raise-syntax-error 'tui "expected (open Namespace)" f)]))

  ; (define-syntax f (make-tui-fun ...)) for a Tui function definition. The return type is quoted
  ; only when it is written.
  (define (fun-declaration def)
    (define-values (name pnames ptypes ret body) (fun-header def))
    #`(define-syntax #,name
        (make-tui-fun '#,name (list #,@(for/list ([t ptypes]) #`(quote-syntax #,t)))
                      #,(and ret #`(quote-syntax #,ret))
                      (quote-syntax #,def))))

  ; (record Name [field : type] ...): declares the type in the local assembly, so later code in the
  ; module is checked against it, and returns the ilasm datum of its class.
  (define (record-declaration f)
    (syntax-parse f
      [(_ name:id [field:id (~datum :) type:id] ...+)
       (define fields
         (for/list ([x (in-list (syntax->list #'(field ...)))]
                    [t (in-list (syntax->list #'(type ...)))])
           (define ty (ann->type t))
           (when (eq? ty 'void) (raise-syntax-error 'tui "a record field cannot be void" t))
           (cons (symbol->string (syntax-e x)) ty)))
       (when (check-duplicates (map car fields))
         (raise-syntax-error 'tui "duplicate field name in record" f))
       (define n (symbol->string (syntax-e #'name)))
       (with-clr f (λ () (declare-record! n fields)))
       (record-class-datum n fields)]
      [_ (raise-syntax-error 'tui "expected (record Name [field : type] ...)" f)]))

  ; The class of a record (name, fields as (name . tui-type)): a constructor that takes each field,
  ; a private field per name, and get_name and set_name accessors.
  (define (record-class-datum name fields)
    (define cname (string->symbol name))
    (define (fname f) (string->symbol (car f)))
    (define (ftype f) (tui-type->il (cdr f)))
    (define (arg-load i)
      (if (< i 4) (list (string->symbol (format "ldarg.~a" i))) (list 'ldarg.s i)))
    (define (accessors f)
      (define t (ftype f))
      (list `(.method (public) ,t ,(string->symbol (string-append "get_" (car f))) () (cil managed)
                      ((ldarg.0) (ldfld ,cname ,(fname f) ,t) (ret)))
            `(.method (public) void ,(string->symbol (string-append "set_" (car f))) (,t)
                      (cil managed)
                      ((ldarg.0) (ldarg.1) (stfld ,cname ,(fname f) ,t) (ret)))))
    (define ctor
      `(.method (public specialname rtspecialname) void .ctor ,(map ftype fields) (cil managed)
                ((ldarg.0) (call void (mscorlib System.Object) .ctor ())
                 ,@(append* (for/list ([f (in-list fields)] [i (in-naturals 1)])
                              `((ldarg.0) ,(arg-load i) (stfld ,cname ,(fname f) ,(ftype f)))))
                 (ret))))
    `(.class (public auto ansi sealed) ,cname
             (,@(for/list ([f (in-list fields)]) `(.field (private) ,(ftype f) ,(fname f)))
              ,ctor
              ,@(append-map accessors fields)))))

;; Phase 1 queries for user macros: the type checker, and the CLR metadata behind a type.
(begin-for-syntax
  ; The Tui type of a typed expression, from the type checker.
  (define (tui-type-of stx)
    (define-values (ty _code) (tc stx))
    ty)

  ; The Tui type named by an identifier in a macro argument, or a syntax error.
  (define (tui-resolve-type id)
    (or (resolve-type-name id) (raise-syntax-error 'tui "unknown type" id)))

  ; Whether the type t is a class with a public constructor that takes no arguments.
  (define (tui-has-default-ctor? t)
    (and (pair? t)
         (ormap (λ (m) (null? (clr-method-params m))) (find-constructors (class-of t)))))

  ; The return type of the first public instance method named name on the class type t, read from
  ; the metadata of the closed type, or #f when there is no such method.
  (define (tui-member-return-type t name)
    (and (pair? t)
         (let ([groups (find-instance-methods t name)])
           (for*/first ([g (in-list groups)] [m (in-list g)] #:when (zero? (clr-method-arity m)))
             (clr-method-ret m)))))

  ; The number of public static methods named method-name on the type named type-name (a source
  ; name such as "Console"). Every overload counts.
  (define (tui-static-overloads type-name method-name)
    (length (or (find-static-methods type-name method-name) '())))

  ; The number of public instance methods named name on t and its base types. Every overload counts.
  (define (tui-instance-overloads t name)
    (for/sum ([g (in-list (find-instance-methods t name))]) (length g)))

  ; The distinct public instance method names of the class type t, sorted. Constructors are left out.
  (define (tui-member-names t)
    (sort (remove-duplicates
           (for/list ([m (in-list (type-methods (class-of t)))]
                      #:unless (clr-method-static? m)
                      #:unless (equal? (clr-method-name m) ".ctor"))
             (clr-method-name m)))
          string<?))
  )

;; Syntax classes that give user macros the Tui types of their arguments. A pattern variable x of
;; class typedExpr has x.Type, the type the checker gives the expression. typeName matches a type
;; name and has x.Type, the type it names. Richer constraints, such as typedList, are built with
;; define-type-syntax-class (see private/type-syntax-class.rkt).
; The attribute expressions run at phase 1, so the classes go in a begin-for-syntax beside the helpers.
(begin-for-syntax
  (define-syntax-class typedExpr
    #:attributes (Type)
    (pattern e
      #:attr Type (tui-type-of #'e)))

  (define-syntax-class typeName
    #:attributes (Type)
    (pattern t:id
      #:attr Type (tui-resolve-type #'t))))

; The type-pattern solver reads expression types through this hook (see type-patterns.rkt).
(begin-for-syntax
  (set-type-of-hook! tui-type-of))

;; ---------------------------------------------------------------------------------------------
;; Module and REPL

(define-syntax (tui-module-begin stx)
  (syntax-parse stx
    [(_ form ...)
     (define forms (syntax->list #'(form ...)))
     (define (of kind) (filter (λ (f) (eq? (form-kind f) kind)) forms))
     (define refs (cons mscorlib-path (map using-path (of 'using))))
     (define opens (cons "System" (map open-namespace (of 'open))))
     (tui-configure! refs opens)
     (reset-local-declarations!)
     (set-box! lifted-classes '())
     (hash-remove! tooltip-tables (syntax-source stx))
     (hash-remove! scope-records (syntax-source stx))
     (define defs (of 'define))
     (define vals (of 'value))
     (record-line-source! (syntax-source stx) (append defs vals (of 'expr)))
     (define val-names (for/list ([v vals]) (list-ref (syntax->list v) 1)))
     (define val-decls
       (for/list ([v vals] [i (in-naturals)])
         (let* ([l (syntax->list v)]
                [name (list-ref l 1)]
                [ann (and (= (length l) 5) (list-ref l 3))])
           #`(define-syntax #,name
               (make-tui-global '#,(syntax-e name) #,(and ann #`(quote-syntax #,ann))
                                (quote-syntax #,v) '#,(map syntax-e (take val-names i)))))))
     (define val-methods
       (for/list ([v vals] [i (in-naturals)])
         #`(tui-value-decls #,v #,@(take val-names i))))
     (define val-inits
       (for/list ([v vals])
         #`(#,(list-ref (syntax->list v) 1))))
     (with-syntax ([prog (datum->syntax stx '%tui-program)]
                   ; records are declared before any function or value is checked
                   [(rec ...) (for/list ([r (of 'record)])
                                (datum->syntax stx (record-declaration r)))]
                   [(decl ...) (append (map fun-declaration defs) val-decls)]
                   [(name ...) (for/list ([d defs])
                                 (let-values ([(n p t r b) (fun-header d)]) n))]
                   [(def ...) defs]
                   [(vdecl ...) val-methods]
                   [(vinit ...) val-inits]
                   [(pass ...) (of 'passthrough)]
                   [(stmt ...) (of 'expr)])
       #`(#%module-begin
          pass ...
          ; visiting a required Tui module resets the shared configuration, so set it again here
          (begin-for-syntax (tui-configure! '#,refs '#,opens))
          decl ...
          (define prog
            (tui-lifting
             (let ([p (make-tui-program (tui-reference-list)
                                        (append (list (cons 'name (tui-method def)) ...) vdecl ...)
                                        (tui-main (vinit ...) stmt ...))])
               (tui-program-add-class! p (quote rec)) ...
               p)))
          ; after prog, so every method and expression of the module has been recorded
          (define %tui-tooltips (tui-tooltips #,(datum->syntax #f (syntax-source stx))))
          (define %tui-scopes (tui-scopes #,(datum->syntax #f (syntax-source stx))))
          (provide (rename-out [prog tui-program]))
          (module+ main (run-tui-program prog))))]))

(define-syntax (tui-reference-list stx)
  #`(quote #,(tui-references)))

; The scope records of the module are logged on the tui-scopes topic. The data is (source records
; references opens), where each record is (vector name type-text position span start end). Positions
; are 1-based. The references and opens are those the module was configured with, so a helper
; process can answer member queries for the same environment.
(define-syntax (tui-scopes stx)
  (syntax-case stx ()
    [(_ src)
     (let* ([recs (hash-ref scope-records (syntax-e #'src) make-hash)]
            [data (list (syntax-e #'src) (hash-values recs) (tui-references) (tui-opens))])
       (log-message tui-scopes-logger 'info 'tui-scopes "Tui scopes" data)
       #''#f)]))

; Logs the module's tooltips on online-check-syntax for racket-langserver, and keeps them in the
; expansion for Check Syntax. The carrier's 'mouse-over-tooltips property holds the leaves.
(define-syntax (tui-tooltips stx)
  (syntax-case stx ()
    [(_ src)
     (let* ([tbl (hash-ref tooltip-tables (syntax-e #'src) make-hash)]
            [carrier (syntax-property (datum->syntax #f 'tui-tooltips)
                                      'mouse-over-tooltips (tooltip-vectors tbl))])
       (log-message online-check-syntax-logger 'info 'online-check-syntax
                    tooltip-log-message (list carrier))
       (with-syntax ([c carrier])
         #'(quote c)))]))

; The field and the initializer of a module value, as (name . datum) pairs. earlier are the names
; of the values defined before it.
(define-syntax (tui-value-decls stx)
  (syntax-case stx ()
    [(_ def earlier ...)
     (let-values ([(init-method field) (value-parts #'def (syntax->list #'(earlier ...)))])
       (define name (syntax-e (cadr (syntax->list #'def))))
       (with-syntax ([pairs (datum->syntax #f (list (cons name field)
                                                    (cons (value-init-name name) init-method)))])
         #'(quote pairs)))]))

; Adds the lambda classes compiled so far to p. Use it only in a let body that follows the
; expression which compiled them (see tui-lifting), so the expansion order is right.
(define-syntax (tui-lifts-into stx)
  (syntax-case stx ()
    [(_ p)
     (let ([ids (reverse (unbox lifted-classes))])
       (set-box! lifted-classes '())
       (with-syntax ([(add ...) (for/list ([id (in-list ids)])
                                  #`(tui-program-add-class! p #,id))])
         #'(begin (void) add ...)))]))

; The value of e, after the lambda bodies that e compiled are added to it.
(define-syntax (tui-lifting stx)
  (syntax-case stx ()
    [(_ e) #'(let ([p e]) (tui-lifts-into p) p)]))

(define-syntax (tui-method stx)
  (syntax-case stx ()
    [(_ def) #`(quote #,(datum->syntax #f (compile-function #'def)))]))

(define-syntax (tui-main stx)
  (syntax-case stx ()
    [(_ (init ...) form ...)
     (let ([inits (for/list ([i (in-list (syntax->list #'(init ...)))])
                    (syntax-case i ()
                      [(n) (list (syntax-e #'n) (global-result-type (syntax-local-value #'n) #'n))]))])
       #`(quote #,(datum->syntax #f (compile-main stx (syntax->list #'(form ...)) #f inits))))]))

(define-syntax (tui-main/print stx)
  (syntax-case stx ()
    [(_ form) #`(quote #,(datum->syntax #f (compile-main stx (list #'form) #t)))]))

; Each REPL interaction is type checked against the module's functions, compiled into a fresh
; entry point, assembled together with the module's methods, and run.
(define-syntax (tui-top-interaction stx)
  (syntax-parse stx
    [(_ . form)
     (define prog (datum->syntax stx '%tui-program))
     (set-box! lifted-classes '())
     (case (form-kind #'form)
       [(define)
        (define-values (name p t r b) (fun-header #'form))
        #`(begin #,(fun-declaration #'form)
                 (let ([v (tui-program-add-method! #,prog '#,name (tui-method form))])
                   (tui-lifts-into #,prog)
                   v))]
       [(using)
        (define p (using-path #'form))
        #`(begin (begin-for-syntax (tui-add-reference! #,p))
                 (tui-program-add-reference! #,prog #,p))]
       [(open) #`(begin-for-syntax (tui-add-open! #,(open-namespace #'form)))]
       [(passthrough) #'form]
       [(value) (raise-syntax-error 'tui "module values can only be defined in a module" #'form)]
       [else #`(let ([m (tui-main/print form)])
                 (tui-lifts-into #,prog)
                 (run-tui-main #,prog m))])]))

;; ---------------------------------------------------------------------------------------------
;; Expressions

(define-tui-form (tui-datum stx)
  (syntax-parse stx
    [(_ . d)
     (define v (syntax-e #'d))
     (cond
       [(boolean? v) (il 'bool `((ldc.i4 ,(if v 1 0))))]
       [(exact-integer? v)
        (cond
          [(<= (- (expt 2 31)) v (sub1 (expt 2 31))) (il 'int32 `((ldc.i ,v)))]
          [(<= (- (expt 2 63)) v (sub1 (expt 2 63))) (il 'int64 `((ldc.i8 ,v)))]
          [else (raise-syntax-error 'tui "integer literal does not fit in int64" #'d)])]
       [(flonum? v) (il 'float64 `((ldc.r8 ,v)))]
       [(string? v) (il 'string `((ldstr ,v)))]
       [(char? v) (il 'char `((ldc.i ,(char->integer v))))]
       [else (raise-syntax-error 'tui "unsupported literal" #'d)])]))

(define-tui-form (tui-top stx)
  (syntax-parse stx
    [(_ . id)
     ; A bare a.b reads a local's property (xs.Count) or a static property or field (Console.Out,
     ; System.Globalization.NumberStyles.Integer).
     (if (regexp-match? #rx"\\." (symbol->string (syntax-e #'id)))
         #'(tui-app id)
         (raise-syntax-error 'tui "unbound identifier" #'id))]))

; A bare null takes its type from the expected type (set by tc/expect), which must be a reference
; type. Its code is the same whatever the type is.
(define-tui-form (tui-null stx)
  (define expected (syntax-property stx 'tui-expected))
  (cond
    [(not expected) (raise-syntax-error 'tui "null needs an expected reference type" stx)]
    [(not (reference-type? expected))
     (raise-syntax-error 'tui (format "null cannot have type ~a" (tui-type->string expected)) stx)]
    [else (il expected '((ldnull)))]))

; (null-of T) is a null of the reference type T, for places with no expected type, such as a let.
(define-tui-form (tui-null-of stx)
  (syntax-parse stx
    [(_ t:typeName)
     (unless (reference-type? (attribute t.Type))
       (raise-syntax-error 'tui (format "null-of needs a reference type, got ~a"
                                        (tui-type->string (attribute t.Type)))
                           #'t))
     (il (attribute t.Type) '((ldnull)))]))

; (typeof T) is the System.Type of the type name T: a ldtoken of T, passed to Type.GetTypeFromHandle.
; The token operand is a primitive, or the name of a class with no type arguments.
(define-tui-form (tui-typeof stx)
  (syntax-parse stx
    [(_ t:typeName)
     (define ty (attribute t.Type))
     (when (eq? ty 'void) (raise-syntax-error 'tui "typeof needs a type, got void" #'t))
     (unless (or (symbol? ty) (and (pair? ty) (eq? (car ty) 'class) (null? (class-args ty))))
       (raise-syntax-error 'tui (format "typeof supports primitives and classes without type arguments, got ~a"
                                        (tui-type->string ty))
                           #'t))
     (define operand (if (symbol? ty) (tui-type->il ty) (cadr (tui-type->il ty))))
     (define sys-type (with-clr stx (λ () (string->tui-type "System.Type"))))
     (define handle-type (with-clr stx (λ () (string->tui-type "System.RuntimeTypeHandle"))))
     (il sys-type
         `((ldtoken ,operand)
           (call ,(tui-type->il sys-type) ,(owner->il sys-type) GetTypeFromHandle
                 (,(tui-type->il handle-type)))))]))

; (null? e) is true when the reference e is null.
(define-tui-form (tui-null? stx)
  (syntax-parse stx
    [(_ a)
     (when (bare-null? #'a) (raise-syntax-error 'tui "null? needs a value, not a bare null" #'a))
     (define-values (ty code) (tc #'a))
     (unless (reference-type? ty)
       (raise-syntax-error 'tui (format "null? needs a reference type, got ~a" (tui-type->string ty))
                           #'a))
     (il 'bool (append code '((ldnull) (ceq))))]))

; (Type.Method arg ...) calls a static .NET method; (.Method obj arg ...) calls an instance one.
(define-tui-form (tui-app stx)
  (syntax-parse stx
    [(_ head:id arg ...)
     (define full (symbol->string (syntax-e #'head)))
     (define targs-text (regexp-match #rx"^(.*)<(.*)>$" full))
     (define name (if targs-text (cadr targs-text) full))
     (define explicit (if targs-text (explicit-targs #'head (caddr targs-text)) '()))
     (define args (syntax->list #'(arg ...)))
     (cond
       [(regexp-match #rx"^\\.([^.]+)$" name)
        => (λ (m) (compile-instance-call stx #'head (cadr m) args explicit))]
       [(and (dotted-match name) (local-receiver #'head name))
        => (λ (_)
             (define m (dotted-match name))
             (compile-instance-call stx #'head (caddr m)
                                    (cons (segment-syntax #'head (cadr m)) args) explicit))]
       [(regexp-match #rx"^([0-9]+(?:\\.[0-9]+)?)\\.([^.]+)$" name)
        => (λ (m)
             (compile-instance-call stx #'head (caddr m)
                                    (cons (literal-syntax #'head (string->number (cadr m)) (cadr m))
                                          args)
                                    explicit))]
       [(regexp-match #rx"^(.+)\\.([^.]+)$" name)
        => (λ (m)
             (define field (and (null? args)
                                (with-clr #'head (λ () (find-static-field (cadr m) (caddr m))))))
             (if field
                 (static-field-read field)
                 (compile-static-call stx #'head (cadr m) (caddr m) args explicit)))]
       [else (raise-syntax-error 'tui "unbound function" #'head)])]
    [(_ head . _) (raise-syntax-error 'tui "only named functions can be called" stx #'head)]))

(begin-for-syntax
  ; The explicit type arguments of a call, as in Enumerable.Where<int32>. A _ is #f, to be inferred.
  (define (explicit-targs head text)
    (for/list ([s (in-list (split-type-args text))])
      (cond
        [(equal? s "_") #f]
        [else
         (or (with-clr head (λ () (string->tui-type s)))
             (raise-syntax-error 'tui (format "unknown type argument ~a" s) head))])))

  ; A head or identifier a.b: the match, or #f. a and b are non-empty and contain no dots.
  (define (dotted-match name)
    (regexp-match #rx"^([^.]+)\\.([^.]+)$" name))

  ; An identifier for the part a of a.b, with the head's context and a span of just that part.
  (define (segment-syntax head text)
    (datum->syntax head (string->symbol text)
                   (list (syntax-source head) (syntax-line head) (syntax-column head)
                         (syntax-position head) (string-length text))))

  ; The delegate type of head when it names a local or a module value of delegate type, or #f.
  ; A call (f x) on such a name is a call of its Invoke method.
  (define (delegate-value-type head)
    (define v (and (identifier? head) (syntax-local-value head (λ () #f))))
    (define t (cond [(tui-var? v) (tui-var-type v)]
                    [(tui-global? v) (global-result-type v head)]
                    [else #f]))
    (and t (delegate-type? t) t))

  ; A literal receiver with the span of its text, so errors point at it.
  (define (literal-syntax head value text)
    (datum->syntax head value
                   (list (syntax-source head) (syntax-line head) (syntax-column head)
                         (syntax-position head) (string-length text))))

  ; The local or parameter (a tui-var) that a in a.b names, or #f. A local shadows a type.
  (define (local-receiver head name)
    (define m (dotted-match name))
    (define v (and m (syntax-local-value (segment-syntax head (cadr m)) (λ () #f))))
    (and (tui-var? v) v))

  ; A bare name with no arguments reads a property when there is no method of that name.
  (define (compile-static-call stx head type-name method-name args explicit)
    (define-values (tys codes) (tc-args args))
    (define (lookup name) (with-clr head (λ () (find-static-methods type-name name))))
    (define ms0 (lookup method-name))
    (unless ms0 (raise-syntax-error 'tui (format "unknown type ~a" type-name) head))
    (define property? (null? args))
    (define ms (if (and property? (null? ms0)) (lookup (string-append "get_" method-name)) ms0))
    (when (null? ms)
      (raise-syntax-error 'tui (format "~a has no public static ~a ~a" type-name
                                       (if property? "method or property" "method") method-name)
                          head))
    (define-values (m convs)
      (with-clr stx (λ () (resolve-overload (list ms) tys
                                            (format "~a.~a" type-name method-name) explicit))))
    (define-values (m* arg-code) (finish-call stx tys codes convs m))
    (il (clr-method-ret m*) (append arg-code (list (clr-call-instr 'call m*)))))

  ; The value of a public static field. A literal (a constant, or an enum member) is pushed as its
  ; value; any other field is loaded from its owner.
  (define (static-field-read f)
    (define ty (clr-field-type f))
    (il ty (if (clr-field-literal? f)
               (literal-instructions ty (clr-field-value f))
               (list `(ldsfld ,(owner->il (clr-field-owner f)) ,(string->symbol (clr-field-name f))
                              ,(tui-type->il ty))))))

  ; A primitive receiver is boxed and dispatched through System.Object (ToString and friends). A
  ; struct or enum receiver is unboxed to a managed pointer, the `this` its own methods take. A
  ; method inherited from a reference base class (ToString from Enum) takes an object reference, so
  ; that receiver stays boxed.
  (define (compile-instance-call stx head method-name args explicit)
    (when (null? args)
      (raise-syntax-error 'tui (format ".~a needs a receiver" method-name) stx))
    (define-values (rty rcode) (tc (car args)))
    (define-values (tys codes) (tc-args (cdr args)))
    (define-values (owner recv-code boxed-code)
      (cond
        [(primitive-value-type? rty)
         (define boxed (append rcode `((box ,(tui-type->il rty)))))
         (values 'object boxed boxed)]
        [(value-type? rty)
         (define il (tui-type->il rty))
         (values rty (append rcode `((box ,il) (unbox ,il))) (append rcode `((box ,il))))]
        [(eq? rty 'void) (raise-syntax-error 'tui "receiver has type void" (car args))]
        [else (values rty rcode #f)]))
    (define (lookup name) (with-clr head (λ () (find-instance-methods owner name))))
    (define property? (null? (cdr args)))
    (define groups0 (lookup method-name))
    (define groups (if (and property? (null? groups0)) (lookup (string-append "get_" method-name)) groups0))
    (when (null? groups)
      (raise-syntax-error 'tui (format "~a has no public instance ~a ~a"
                                       (tui-type->string owner)
                                       (if property? "method or property" "method") method-name)
                          head))
    (define-values (m convs)
      (with-clr stx (λ () (resolve-overload groups tys
                                            (format "~a.~a" (tui-type->string owner) method-name)
                                            explicit))))
    (define-values (m* arg-code) (finish-call stx tys codes convs m))
    (define recv (if (and boxed-code (not (equal? (clr-method-owner m) rty))) boxed-code recv-code))
    (il (clr-method-ret m*)
        (append recv arg-code
                (list (clr-call-instr 'callvirt m*))))))

; (new Type arg ...) calls a public constructor.
(define-tui-form (tui-new stx)
  (syntax-parse stx
    [(_ type:id arg ...)
     (define t (class-of (ann->type #'type)))
     (unless (pair? t) (raise-syntax-error 'tui "new needs a class type" #'type))
     (define-values (tys codes) (tc-args (syntax->list #'(arg ...))))
     (define-values (m convs)
       (with-clr stx (λ () (resolve-overload (list (find-constructors t)) tys
                                             (format "new ~a" (tui-type->string t))))))
     (define-values (m* arg-code) (finish-call stx tys codes convs m))
     (il (ann->type #'type)
         (append arg-code
                 `((newobj ,(owner->il t) ,(clr-method-il-params m*)))))]))

; (list-of e ...) builds a System.Collections.Generic.List<T>, where T is the type of the first
; element. The expansion is looked up in the type environment: the List definition is instantiated
; at T, its parameterless constructor and Add are chosen from the closed type, and every other
; element is checked against T, so a mismatch is a compile error at that element.
(begin-for-syntax
  (define (list-code stx elem-codes elem-type)
    (define inst (with-clr stx (λ () (generic-instance "System.Collections.Generic.List"
                                                       (list elem-type)))))
    (unless inst
      (raise-syntax-error 'tui (format "cannot form a List of ~a" (tui-type->string elem-type))
                          stx))
    (define-values (ctor _c)
      (with-clr stx (λ () (resolve-overload (list (find-constructors inst)) '() "new List"))))
    (define-values (add _a)
      (with-clr stx (λ () (resolve-overload (find-instance-methods inst "Add") (list elem-type)
                                            "List.Add"))))
    (il inst (append `((newobj ,(owner->il inst) ,(clr-method-il-params ctor)))
                     (append* (for/list ([c (in-list elem-codes)])
                                (append '((dup)) c (list (clr-call-instr 'callvirt add)))))))))

(define-tui-form (tui-list-of stx)
  (syntax-parse stx
    [(_) (raise-syntax-error
          'tui "list-of needs an element to infer the type; for an empty list use (new System.Collections.Generic.List<T>)"
          stx)]
    [(_ e0 e ...)
     (define-values (elem c0) (tc #'e0))
     (when (eq? elem 'void) (raise-syntax-error 'tui "list elements cannot be void" #'e0))
     (define rest-codes (for/list ([x (in-list (syntax->list #'(e ...)))]) (tc/expect x elem)))
     (list-code stx (cons c0 rest-codes) elem)]))

; (array T e ...) builds a single-dimensional array of T, holding the elements in order. Each
; element is checked against T.
(define-tui-form (tui-array stx)
  (syntax-parse stx
    [(_ et:id e ...)
     (define elem (ann->type #'et))
     (define elem-il (tui-type->il elem))
     (define stores
       (for/list ([x (in-list (syntax->list #'(e ...)))] [k (in-naturals)])
         (append '((dup)) `((ldc.i4 ,k)) (tc/expect x elem) `((stelem ,elem-il)))))
     (il (list 'szarray elem)
         (append `((ldc.i4 ,(length stores)) (newarr ,elem-il)) (append* stores)))]))

; (new-array T n) builds an array of n elements of T, each set to the default value.
(define-tui-form (tui-new-array stx)
  (syntax-parse stx
    [(_ et:id n)
     (define elem (ann->type #'et))
     (il (list 'szarray elem) (append (tc/expect #'n 'int32) `((newarr ,(tui-type->il elem)))))]))

; (aref a i) reads element i of the array a.
(define-tui-form (tui-aref stx)
  (syntax-parse stx
    [(_ a i)
     (define-values (aty acode) (tc #'a))
     (unless (szarray-type? aty)
       (raise-syntax-error 'tui (format "~a is not an array" (tui-type->string aty)) #'a))
     (define elem (szarray-elem aty))
     (il elem (append acode (tc/expect #'i 'int32) `((ldelem ,(tui-type->il elem)))))]))

; The length of the array a, as an int32. Used by the lowering of for-each.
(define-tui-form (tui-alen stx)
  (syntax-parse stx
    [(_ a)
     (define-values (aty acode) (tc #'a))
     (il 'int32 (append acode '((ldlen) (conv.i4))))]))

; (tui-as T e) checks that e converts to T, and has type T. The error is reported at T, which
; is the loop variable's annotation.
(define-tui-form (tui-as stx)
  (syntax-parse stx
    [(_ t:id e)
     (define to (ann->type #'t))
     (define-values (ty code) (tc #'e))
     (define c (coercion ty to))
     (unless c
       (raise-syntax-error 'tui (format "type mismatch: expected ~a, got ~a"
                                        (tui-type->string to) (tui-type->string ty))
                           #'t))
     (il to (append code (cdr c)))]))

(begin-for-syntax
  ; A call of the instance method name on the enumerator en. owner is the type that declares the
  ; method, or #f for en's own type. The result has the method's return type.
  (define (enumerator-call stx en owner name)
    (define-values (ty code) (tc en))
    (define-values (m _c)
      (with-clr stx (λ () (resolve-overload (find-instance-methods (or owner ty) name) '() name))))
    (il (clr-method-ret m) (append code (list (clr-call-instr 'callvirt m)))))

  ; The for-each expansion: a temporary holds the sequence, and the body runs with x bound to
  ; each element in turn. anns is () or (T), the loop variable's annotation.
  (define (lower-for-each stx x anns e bodies)
    (define src (syntax-property (datum->syntax x (gensym 'src)) 'tui-expr e))
    (inherit-stmt stx
     (quasisyntax/loc stx
      (tui-let ([#,src #,e]) (tui-for-each-loop #,src #,x #,anns #,@bodies))))))

; The IEnumerator<T> of a sequence, from GetEnumerator on IEnumerable<T>. An array implements
; IEnumerable<T>, so it needs no conversion.
(define-tui-form (tui-enumerator stx)
  (syntax-parse stx
    [(_ src)
     (define-values (ty code) (tc #'src))
     (define elem (enumerable-element ty))
     (unless elem
       (raise-syntax-error 'tui (format "for-each needs an array or IEnumerable<T>, got ~a"
                                        (tui-type->string ty))
                           #'src))
     (define ienum (with-clr stx (λ () (generic-instance "System.Collections.Generic.IEnumerable"
                                                         (list elem)))))
     (define ienumr (with-clr stx (λ () (generic-instance "System.Collections.Generic.IEnumerator"
                                                          (list elem)))))
     (define-values (m _c)
       (with-clr stx (λ () (resolve-overload (find-instance-methods ienum "GetEnumerator") '()
                                             "GetEnumerator"))))
     (il ienumr (append code (list (clr-call-instr 'callvirt m))))]))

; MoveNext is declared on the non-generic IEnumerator, which IEnumerator<T> inherits.
(define-tui-form (tui-enum-move stx)
  (syntax-parse stx
    [(_ en)
     (enumerator-call stx #'en
                      (with-clr stx (λ () (generic-instance "System.Collections.IEnumerator" '())))
                      "MoveNext")]))

(define-tui-form (tui-enum-current stx)
  (syntax-parse stx
    [(_ en) (enumerator-call stx #'en #f "get_Current")]))

; (for-each [x e] body ...+) runs body for each element of e, with x bound to it. The loop
; variable may be annotated, as in (for-each [x : T e] body ...+), and the annotation is checked.
(define-tui-form (tui-for-each stx)
  (syntax-parse stx
    [(_ [x:id (~datum :) t:id e] body ...+) (lower-for-each stx #'x #'(t) #'e #'(body ...))]
    [(_ [x:id e] body ...+) (lower-for-each stx #'x #'() #'e #'(body ...))]
    [_ (raise-syntax-error 'tui "expected (for-each [x e] body ...+) or (for-each [x : T e] body ...+)"
                           stx)]))

; The loop itself, once the sequence is bound to src. An array is walked by index, and any other
; sequence by its enumerator.
(define-tui-form (tui-for-each-loop stx)
  (syntax-parse stx
    [(_ src:id x:id (t:id ...) body ...+)
     (define ty (tui-var-type (syntax-local-value #'src)))
     (define anns (syntax->list #'(t ...)))
     (define (bind value)
       (if (null? anns)
           (quasisyntax/loc stx (tui-let ([x #,value]) body ...))
           (quasisyntax/loc stx (tui-let ([x (tui-as #,(car anns) #,value)]) body ...))))
     (cond
       [(szarray-type? ty)
        (define i (datum->syntax #'src (gensym 'i)))
        (inherit-stmt stx
         (quasisyntax/loc stx
          (tui-let ([#,i (tui-datum . 0)])
            (tui-while (tui-< #,i (tui-alen src))
              (tui-begin #,(bind #`(tui-aref src #,i))
                         (tui-set! #,i (tui-+ #,i (tui-datum . 1))))))))]
       [(enumerable-element ty)
        (define en (datum->syntax #'src (gensym 'en)))
        (inherit-stmt stx
         (quasisyntax/loc stx
          (tui-let ([#,en (tui-enumerator src)])
            (tui-while (tui-enum-move #,en)
              #,(bind #`(tui-enum-current #,en))))))]
       [else
        (raise-syntax-error 'tui (format "for-each needs an array or IEnumerable<T>, got ~a"
                                         (tui-type->string ty))
                            (or (syntax-property #'src 'tui-expr) #'src))])]))

(define-tui-form (tui-begin stx)
  (syntax-parse stx
    [(_) (il 'void '())]
    [(_ e ... last)
     (define code
       (append* (for/list ([e (syntax->list #'(e ...))])
                  (define-values (ty code) (tc (inherit-stmt stx e)))
                  (append (line-marker e)
                          code
                          (if (memq ty '(void never)) '() '((pop)))))))
     (define-values (ty last-code) (tc (inherit-stmt stx #'last)))
     (il ty (append code (line-marker #'last) last-code))]))

(define-tui-form (tui-let stx)
  (syntax-parse stx
    [(_ ([x:id e] ...) body ...+)
     (define-values (tys codes)
       (for/lists (tys codes) ([s (in-list (syntax->list #'(e ...)))]) (tc (inherit-stmt stx s))))
     (for ([ty tys] [e (syntax->list #'(e ...))])
       (when (memq ty '(void never))
         (raise-syntax-error 'tui (if (eq? ty 'void)
                                      "cannot bind a void value"
                                      "cannot bind a value that never completes")
                             e)))
     (define xs (syntax->list #'(x ...)))
     (for ([x (in-list xs)] [t (in-list tys)]) (record-tooltip! x t))
     (let-values ([(start end) (forms-span (syntax->list #'(body ...)))])
       (for ([x (in-list xs)] [t (in-list tys)]) (record-scope! x (tui-type->string t) start end)))
     (define ords (for/list ([x (in-list xs)]) (next-ordinal!)))
     (define cells? (for/list ([ord (in-list ords)]) (cell-ordinal? ord)))
     (define slots (for/list ([x (in-list xs)] [ty tys] [c? cells?])
                     (alloc-local! stx (if c? (cell-of ty) ty) (syntax-e x))))
     ; A cell is created each time the let runs, so each run (for example, each loop iteration)
     ; gets a fresh variable.
     ; A cell's value is computed before the cell's array is pushed, so that the value can be a try
     ; (which needs an empty stack).
     (define init
       (append*
        (for/list ([c codes] [slot slots] [ty tys] [c? cells?])
          (if c?
              (let ([tmp (alloc-local! stx ty)])
                (append c
                        (list (var-instr 'st 'local tmp))
                        (new-cell-code ty)
                        (list (var-instr 'st 'local slot)
                              (var-instr 'ld 'local slot)
                              '(ldc.i4.0)
                              (var-instr 'ld 'local tmp)
                              `(stelem ,(tui-type->il ty)))))
              (append c (list (var-instr 'st 'local slot)))))))
     (define-values (ty code)
       (tc/bind (for/list ([x (in-list xs)] [slot (in-list slots)] [t (in-list tys)]
                           [c? (in-list cells?)] [ord (in-list ords)])
                  (list x (if c? 'cell 'local) slot t ord))
                (inherit-stmt stx #'(tui-begin body ...))))
     (il ty (append init code))]))

(define-tui-form (tui-set! stx)
  (syntax-parse stx
    [(_ (h:id a i) e) #:when (free-identifier=? #'h #'tui-aref)
     (define-values (aty acode) (tc #'a))
     (unless (szarray-type? aty)
       (raise-syntax-error 'tui (format "~a is not an array" (tui-type->string aty)) #'a))
     (define elem (szarray-elem aty))
     (il 'void (append acode (tc/expect #'i 'int32) (tc/expect #'e elem #:assign? #t)
                       `((stelem ,(tui-type->il elem)))))]
    [(_ x:id e)
     (define v (syntax-local-value #'x (λ () #f)))
     (define name (symbol->string (syntax-e #'x)))
     (cond
       [(and (not (tui-var? v)) (dotted-match name) (local-receiver #'x name))
        ; (set! p.x e) is the setter call (p.set_x e), so it is checked like any other call
        (define m (dotted-match name))
        (define setter (datum->syntax #'x (string->symbol (format "~a.set_~a" (cadr m) (caddr m)))
                                      #'x))
        (define-values (ty code) (tc (datum->syntax #'x (list setter #'e) #'x) #f #'x))
        (il ty code)]
       [(tui-global? v)
        (check-global-scope v #'x)
        (define ty (global-result-type v #'x))
        (record-tooltip! #'x ty)
        (il 'void (append (tc/expect #'e ty #:assign? #t)
                          `((stsfld Program ,(tui-global-name v) ,(tui-type->il ty)))))]
       [else
        (unless (tui-var? v) (raise-syntax-error 'tui "not a variable" #'x))
        (record-tooltip! #'x (tui-var-type v))
        (il 'void (store-var v #'x (tc/expect #'e (tui-var-type v) #:assign? #t)))])]))

; (fn Delegate (param ...) body ...+), or (fn (param ...) body ...+) when the expected type of the
; form (set by tc/expect as 'tui-expected) is the delegate type. A param is id or [id : type]. With
; no delegate type, every param is annotated and the delegate is Func or Action of the body type.
(define-tui-form (tui-fn stx)
  (syntax-parse stx
    [(_ dt:id (p ...) body ...+)
     (compile-lambda stx (ann->type #'dt) (lambda-params (syntax->list #'(p ...)))
                     (syntax->list #'(body ...)))]
    [(_ (p ...) body ...+)
     (compile-lambda stx (syntax-property stx 'tui-expected) (lambda-params (syntax->list #'(p ...)))
                     (syntax->list #'(body ...)))]
    [_ (raise-syntax-error 'tui "expected (fn [Delegate] (param ...) body ...+)" stx)]))


(define-tui-form (tui-if stx)
  (syntax-parse stx
    [(_ c t)
     (define then (inherit-stmt stx #'(tui-begin t (tui-begin))))
     (inherit-stmt stx #`(tui-if c #,then (tui-begin)))]
    [(_ c t e)
     (define ccode (tc/expect (inherit-stmt stx #'c) 'bool))
     (define-values (tty tcode) (tc (inherit-stmt stx #'t)))
     (define-values (ety ecode) (tc (inherit-stmt stx #'e)))
     (define-values (ty tc2 ec2)
       (cond
         [(equal? tty ety) (values tty tcode ecode)]
         [(eq? tty 'never) (values ety tcode ecode)]
         [(eq? ety 'never) (values tty tcode ecode)]
         [(eq? tty 'void) (values 'void tcode (append ecode '((pop))))]
         [(eq? ety 'void) (values 'void (append tcode '((pop))) ecode)]
         [(coercion tty ety) => (λ (c) (values ety (append tcode (cdr c)) ecode))]
         [(coercion ety tty) => (λ (c) (values tty tcode (append ecode (cdr c))))]
         [else (raise-syntax-error 'tui (format "if branches have types ~a and ~a"
                                                (tui-type->string tty) (tui-type->string ety))
                                   stx)]))
     (define else-l (fresh-label! stx 'else))
     (define end-l (fresh-label! stx 'endif))
     (il ty (append ccode `((brfalse ,else-l)) tc2 `((br ,end-l) (label ,else-l))
                    ec2 `((label ,end-l))))]))

(define-tui-form (tui-while stx)
  (syntax-parse stx
    [(_ c body ...)
     (define top (fresh-label! stx 'while))
     (define end (fresh-label! stx 'endwhile))
     (define ccode (tc/expect (inherit-stmt stx #'c) 'bool))
     (define-values (ty bcode) (tc (inherit-stmt stx #'(tui-begin body ... (tui-begin)))))
     (il 'void (append `((label ,top)) ccode `((brfalse ,end)) bcode `((br ,top) (label ,end))))]))

; Exceptions. try lowers to .try clauses (see tui-try). in-catch-handler is true while a catch
; handler of the current method is expanded, which is where rethrow is valid.
(begin-for-syntax
  (define in-catch-handler (make-parameter #f))

  (define (exception-type stx)
    (with-clr stx (λ () (string->tui-type "System.Exception"))))

  ; The keyword (catch or finally) that starts a try clause, or #f for any other form.
  (define (clause-keyword f)
    (define parts (syntax->list f))
    (and parts (pair? parts) (identifier? (car parts))
         (memq (syntax-e (car parts)) '(catch finally))
         (syntax-e (car parts))))

  ; A catch clause: its syntax, the exception variable, its type, its handler forms, and whether
  ; it catches every exception (no type given).
  (struct catch-clause (stx name type forms all?))
  ; A catch clause after expansion: the (asm ty) of its .try clause, the code that stores the
  ; exception object (it is on the stack when the handler starts), its type and its body code.
  (struct handler-part (owner prefix type code))

  (define (parse-catch c)
    (define cc
      (syntax-parse c
        [(_ [e:id (~datum :) t:id] h ...+)
         (catch-clause c #'e (ann->type #'t) (syntax->list #'(h ...)) #f)]
        [(_ e:id h ...+)
         (catch-clause c #'e (exception-type c) (syntax->list #'(h ...)) #t)]
        [_ (raise-syntax-error 'tui "expected (catch [name : Type] handler ...+) or (catch name handler ...+)" c)]))
    (define ty (catch-clause-type cc))
    (unless (coercion ty (exception-type c))
      (raise-syntax-error 'tui (format "catch needs an exception type, got ~a" (tui-type->string ty)) c))
    cc)

  ; The (asm ty) of a .try catch clause. Only a non-generic type from another assembly can be
  ; named there.
  (define (catch-owner stx ty)
    (define il (tui-type->il ty))
    (unless (and (pair? il) (= (length il) 2) (pair? (cadr il)) (= (length (cadr il)) 2))
      (raise-syntax-error 'tui (format "cannot catch ~a" (tui-type->string ty)) stx))
    (cadr il))

  (define (parse-finally c)
    (syntax-parse c
      [(_ f ...+) (syntax->list #'(f ...))]
      [_ (raise-syntax-error 'tui "expected (finally cleanup ...+)" c)])))

(define-tui-form (tui-throw stx)
  (syntax-parse stx
    [(_ e)
     (define exn (exception-type stx))
     (define-values (ty code) (tc (syntax-property #'e 'tui-expected exn) #f #'e))
     (define c (and (not (eq? ty 'never)) (coercion ty exn)))
     (unless c
       (raise-syntax-error 'tui (format "throw needs an exception, got ~a" (tui-type->string ty)) #'e))
     (il 'never (append code (cdr c) '((throw))))]
    [_ (raise-syntax-error 'tui "expected (throw exception)" stx)]))

(define-tui-form (tui-rethrow stx)
  (syntax-parse stx
    [(_)
     (unless (in-catch-handler)
       (raise-syntax-error 'tui "rethrow is only allowed in a catch handler" stx))
     (il 'never '((rethrow)))]
    [_ (raise-syntax-error 'tui "expected (rethrow)" stx)]))

; (try body ...+ clause ...), where a clause is (catch [name : Type] handler ...+), (catch name
; handler ...+) for any exception, or (finally cleanup ...+) as the last clause.
;
; The value of the body and of each handler goes to a result local r, and every branch leaves the
; protected region with leave:
;   ots:  ts:  body; stloc r; leave after-catch
;         te:
;         hs:  stloc ex; handler; stloc r; leave after-catch     (one per catch clause)
;         he:
;   after-catch: leave after-all                                  (only with finally)
;   ote:  fhs: cleanup; endfinally                                (only with finally)
;         fhe:
;   after-all: ldloc r
; The clauses are .try entries over [ts, te), in source order, and the finally is an outer .try
; over [ots, ote). A branch whose type is never ends in throw or rethrow, so it gets no stloc or leave.
(define-tui-form (tui-try stx)
  (unless (syntax-property stx 'tui-stmt)
    (raise-syntax-error 'tui "try must be a statement; its value would be on the evaluation stack here"
                        stx))
  (define-values (body-forms clause-forms)
    (splitf-at (cdr (syntax->list stx)) (λ (f) (not (clause-keyword f)))))
  (when (null? body-forms) (raise-syntax-error 'tui "try needs a body" stx))
  (when (null? clause-forms) (raise-syntax-error 'tui "try needs a catch or finally clause" stx))
  (define kinds (map clause-keyword clause-forms))
  (for ([f (in-list clause-forms)] [k (in-list kinds)] [i (in-naturals 1)])
    (cond
      [(not k) (raise-syntax-error 'tui "expected a catch or finally clause" f)]
      [(and (eq? k 'finally) (< i (length clause-forms)))
       (raise-syntax-error 'tui "finally must be the last clause of try" f)]))
  (define finally? (eq? (last kinds) 'finally))
  (define catches
    (for/list ([c (in-list (if finally? (drop-right clause-forms 1) clause-forms))])
      (parse-catch c)))
  (for ([cc (in-list catches)] [i (in-naturals 1)])
    (when (and (catch-clause-all? cc) (< i (length catches)))
      (raise-syntax-error 'tui "a catch-all clause must be the last catch clause" (catch-clause-stx cc))))
  (define fin-forms (if finally? (parse-finally (last clause-forms)) '()))

  (define-values (bty bcode)
    (tc (syntax-property #`(tui-begin #,@body-forms) 'tui-stmt #t)))

  (define (expand-catch cc)
    (define ety (catch-clause-type cc))
    (define e (catch-clause-name cc))
    (define hforms (catch-clause-forms cc))
    (define owner (catch-owner (catch-clause-stx cc) ety))
    (define ord (next-ordinal!))
    (define cell? (cell-ordinal? ord))
    (define slot (alloc-local! stx (if cell? (cell-of ety) ety) (syntax-e e)))
    (define tmp (and cell? (alloc-local! stx ety)))
    (define-values (start end) (forms-span hforms))
    (record-tooltip! e ety)
    (record-scope! e (tui-type->string ety) start end)
    (define-values (hty hcode)
      (parameterize ([in-catch-handler #t])
        (tc/bind (list (list e (if cell? 'cell 'local) slot ety ord))
                 (syntax-property #`(tui-begin #,@hforms) 'tui-stmt #t))))
    ; The exception object is on the stack. A cell is built from it through tmp, as in tui-let.
    (define prefix
      (if cell?
          (append (list (var-instr 'st 'local tmp))
                  (new-cell-code ety)
                  (list (var-instr 'st 'local slot)
                        (var-instr 'ld 'local slot)
                        '(ldc.i4.0)
                        (var-instr 'ld 'local tmp)
                        `(stelem ,(tui-type->il ety))))
          (list (var-instr 'st 'local slot))))
    (handler-part owner prefix hty hcode))

  (define handlers (for/list ([cc (in-list catches)]) (expand-catch cc)))

  (define target
    (if (eq? bty 'never)
        (or (for/first ([h (in-list handlers)] #:unless (eq? (handler-part-type h) 'never))
              (handler-part-type h))
            'never)
        bty))
  (for ([h (in-list handlers)])
    (define t (handler-part-type h))
    (unless (or (eq? t 'never) (coercion t target))
      (raise-syntax-error 'tui (format "catch handler has type ~a, but the try has type ~a"
                                       (tui-type->string t) (tui-type->string target))
                          stx)))
  (define value? (not (memq target '(void never))))
  (define r (and value? (alloc-local! stx target)))

  (define fin-code
    (cond
      [finally?
       (define-values (fty fcode)
         (parameterize ([in-catch-handler #f])
           (tc (syntax-property #`(tui-begin #,@fin-forms) 'tui-stmt #t))))
       (unless (eq? fty 'void)
         (raise-syntax-error 'tui (format "a finally body must be void, got ~a" (tui-type->string fty))
                             (last clause-forms)))
       fcode]
      [else '()]))

  (define (branch-code ty code)
    (cond
      [(eq? ty 'never) code]
      [else
       (append code
               (cdr (coercion ty target))
               (if value? (list (var-instr 'st 'local r)) '())
               (list `(leave ,after-catch)))]))

  (define ots (and finally? (fresh-label! stx 'tryfinally)))
  (define ote (and finally? (fresh-label! stx 'endtryfinally)))
  (define fhs (and finally? (fresh-label! stx 'finallyhandler)))
  (define fhe (and finally? (fresh-label! stx 'endfinallyhandler)))
  (define ts (fresh-label! stx 'trybody))
  (define te (fresh-label! stx 'endtrybody))
  (define after-catch (fresh-label! stx 'endtry))
  (define after-all (and finally? (fresh-label! stx 'endtryall)))
  (define hs-labels (for/list ([h (in-list handlers)]) (fresh-label! stx 'catchhandler)))
  (define he-labels (for/list ([h (in-list handlers)]) (fresh-label! stx 'endcatchhandler)))

  (define code
    (append
     (if finally? (list `(label ,ots)) '())
     (list `(label ,ts))
     (branch-code bty bcode)
     (list `(label ,te))
     (append*
      (for/list ([h (in-list handlers)] [hs (in-list hs-labels)] [he (in-list he-labels)])
        (append (list `(label ,hs))
                (handler-part-prefix h)
                (branch-code (handler-part-type h) (handler-part-code h))
                (list `(label ,he)))))
     (list `(label ,after-catch))
     (if finally?
         (append `((leave ,after-all) (label ,ote) (label ,fhs))
                 fin-code
                 `((endfinally) (label ,fhe) (label ,after-all)))
         '())
     (if value? (list (var-instr 'ld 'local r)) '())
     (for/list ([h (in-list handlers)] [hs (in-list hs-labels)] [he (in-list he-labels)])
       `(.try (catch ,(handler-part-owner h)) ,ts ,te ,hs ,he))
     (if finally? (list `(.try (finally) ,ots ,ote ,fhs ,fhe)) '())))

  (il target code))

(define-tui-form (tui-and stx)
  (syntax-parse stx
    [(_) (il 'bool '((ldc.i4.1)))]
    [(_ a) (il 'bool (tc/expect #'a 'bool))]
    [(_ a b ...)
     (define f (fresh-label! stx 'and))
     (define end (fresh-label! stx 'endand))
     (il 'bool (append (tc/expect #'a 'bool) `((brfalse ,f))
                       (tc/expect #'(tui-and b ...) 'bool)
                       `((br ,end) (label ,f) (ldc.i4.0) (label ,end))))]))

(define-tui-form (tui-or stx)
  (syntax-parse stx
    [(_) (il 'bool '((ldc.i4.0)))]
    [(_ a) (il 'bool (tc/expect #'a 'bool))]
    [(_ a b ...)
     (define t (fresh-label! stx 'or))
     (define end (fresh-label! stx 'endor))
     (il 'bool (append (tc/expect #'a 'bool) `((brtrue ,t))
                       (tc/expect #'(tui-or b ...) 'bool)
                       `((br ,end) (label ,t) (ldc.i4.1) (label ,end))))]))

(define-tui-form (tui-not stx)
  (syntax-parse stx
    [(_ a) (il 'bool (append (tc/expect #'a 'bool) '((ldc.i4.0) (ceq))))]))

;; Arithmetic and comparison. Mixed numeric operands widen to the widest one.

(begin-for-syntax
  (define (widen stx args)
    (define-values (tys codes) (tc* args))
    (for ([t tys] [a args])
      (unless (numeric-type? t)
        (raise-syntax-error 'tui (format "expected a number, got ~a" (tui-type->string t)) a)))
    (define target (cond [(memq 'float64 tys) 'float64] [(memq 'int64 tys) 'int64] [else 'int32]))
    (values target (for/list ([t tys] [c codes]) (append c (cdr (coercion t target))))))

  (define (arith stx op args)
    (define-values (ty codes) (widen stx args))
    (il ty (append (car codes) (append* (for/list ([c (cdr codes)]) (append c (list op))))))))

(define-tui-form (tui-+ stx)
  (syntax-parse stx
    [(_) (il 'int32 '((ldc.i4.0)))]
    [(_ a b ...+)
     (define args (syntax->list #'(a b ...)))
     (define-values (tys codes) (tc* args))
     (if (andmap (λ (t) (eq? t 'string)) tys)
         ; strings concatenate with String.Concat(string, string)
         (il 'string (append (car codes)
                             (append* (for/list ([c (cdr codes)])
                                        (append c '((call string (mscorlib System.String) Concat
                                                          (string string))))))))
         (arith stx '(add) args))]
    [(_ a) (arith stx '(add) (list #'a))]))

(define-tui-form (tui-- stx)
  (syntax-parse stx
    [(_ a) (let-values ([(ty codes) (widen stx (list #'a))])
             (il ty (append (car codes) '((neg)))))]
    [(_ a b ...+) (arith stx '(sub) (syntax->list #'(a b ...)))]))

(define-tui-form (tui-* stx)
  (syntax-parse stx
    [(_ a ...+) (arith stx '(mul) (syntax->list #'(a ...)))]))

(define-tui-form (tui-/ stx)
  (syntax-parse stx
    [(_ a b ...+) (arith stx '(div) (syntax->list #'(a b ...)))]))

(define-tui-form (tui-% stx)
  (syntax-parse stx
    [(_ a b) (arith stx '(rem) (list #'a #'b))]))

(begin-for-syntax
  ; III.3.20-3.23: clt and cgt directly; <= and >= are the negated opposite comparison.
  (define (compare stx ops a b)
    (define-values (ty codes) (widen stx (list a b)))
    (il 'bool (append (car codes) (cadr codes) ops))))

(define-tui-form (tui-< stx) (syntax-parse stx [(_ a b) (compare stx '((clt)) #'a #'b)]))
(define-tui-form (tui-> stx) (syntax-parse stx [(_ a b) (compare stx '((cgt)) #'a #'b)]))
(define-tui-form (tui-<= stx) (syntax-parse stx [(_ a b) (compare stx '((cgt) (ldc.i4.0) (ceq)) #'a #'b)]))
(define-tui-form (tui->= stx) (syntax-parse stx [(_ a b) (compare stx '((clt) (ldc.i4.0) (ceq)) #'a #'b)]))

; Numbers compare by value, strings with String.op_Equality, bool, char and references with ceq.
; A null operand takes the type of the other operand, which must be a reference type.
(define-tui-form (tui-= stx)
  (syntax-parse stx
    [(_ a b)
     #:when (or (bare-null? #'a) (bare-null? #'b))
     (when (and (bare-null? #'a) (bare-null? #'b))
       (raise-syntax-error 'tui "cannot compare null with null" stx))
     (define null-first? (bare-null? #'a))
     (define-values (other-ty other-code) (tc (if null-first? #'b #'a)))
     (unless (reference-type? other-ty)
       (raise-syntax-error 'tui (format "cannot compare ~a with null" (tui-type->string other-ty))
                           stx))
     (define null-code (tc/expect (if null-first? #'a #'b) other-ty))
     (il 'bool (append (if null-first? null-code other-code)
                       (if null-first? other-code null-code)
                       '((ceq))))]
    [(_ a b)
     (define-values (tys codes) (tc* (list #'a #'b)))
     (cond
       [(andmap numeric-type? tys) (compare stx '((ceq)) #'a #'b)]
       [(not (equal? (car tys) (cadr tys)))
        (raise-syntax-error 'tui (format "cannot compare ~a with ~a"
                                         (tui-type->string (car tys)) (tui-type->string (cadr tys)))
                            stx)]
       [(eq? (car tys) 'string)
        (il 'bool (append (car codes) (cadr codes)
                          '((call boolean (mscorlib System.String) op_Equality (string string)))))]
       [(eq? (car tys) 'void) (raise-syntax-error 'tui "cannot compare void values" stx)]
       [else (il 'bool (append (car codes) (cadr codes) '((ceq))))])]))
