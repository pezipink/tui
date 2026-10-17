#lang racket/base
; Tui: a tiny statically typed .NET language. The macro expander is the compiler. Each Tui form is
; a macro that type checks its subforms by expanding them (local-expand stops at the internal
; tui-il form) and produces a typed fragment of Rangi ilasm. Variables are real Racket bindings in
; internal definition contexts, so hygiene and define-syntax-rule macros work unchanged. Calls to
; .NET are resolved at expansion time by reading the referenced assemblies with Rangi's reader.
; The module's program is kept as ilasm datums, assembled with Rangi when it runs.
(require (for-syntax racket/base racket/list racket/string syntax/parse syntax/parse/define
                     "private/clr.rkt")
         syntax/parse/define
         (only-in syntax/parse define-syntax-class pattern)
         "private/runtime.rkt")

(provide (rename-out [tui-module-begin #%module-begin]
                     [tui-top-interaction #%top-interaction]
                     [tui-app #%app] [tui-datum #%datum] [tui-top #%top]
                     [tui-define define] [tui-let let] [tui-set! set!] [tui-if if]
                     [tui-while while] [tui-begin begin] [tui-and and] [tui-or or] [tui-not not]
                     [tui-new new] [tui-list-of list-of] [tui-fn fn]
                     [tui-+ +] [tui-- -] [tui-* *] [tui-/ /] [tui-% %]
                     [tui-< <] [tui-> >] [tui-<= <=] [tui->= >=] [tui-= =])
         ; syntax-rules compiles its patterns at phase 1, so the ellipsis is needed there too
         using open define-syntax-rule define-syntax define-syntax-parser require provide
         for-syntax file
         ... _ (for-syntax ... _)
         (for-syntax tui-type-of tui-member-return-type tui-type->string class-of
                     tui-resolve-type tui-has-default-ctor?
                     typedExpr typedList typeName))

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

; These forms are handled by tui-module-begin and tui-top-interaction.
(define-for-syntax (top-level-only what)
  (module-body-form
   (λ (stx)
     (raise-syntax-error 'tui (format "~a is only allowed at module level or in the REPL" what)
                         stx))))

(define-syntax tui-define (top-level-only "define"))
(define-syntax using (top-level-only "using"))
(define-syntax open (top-level-only "open"))

(begin-for-syntax
  ; Per-method state: local variable types (in slot order) and a label counter. parent is the
  ; enclosing method of a lambda. env lists the (variable . local) pairs that a lambda reads from
  ; its class fields.
  (struct mctx ([locals #:mutable] [labels #:mutable] parent [env #:mutable]))
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

  ; Calls thunk, which compiles one method body and returns (values code locals-or-mctx). Returns
  ; the result of the second pass if the body has lambdas, otherwise the result of the first.
  (define (run-passes thunk)
    (define d (pass #t (make-hasheqv) (make-hasheqv) 0 0))
    (define-values (code0 r0) (parameterize ([current-pass d]) (thunk)))
    (cond
      [(zero? (pass-lambdas d)) (values code0 r0)]
      [else
       (parameterize ([current-pass (pass #f (pass-used d) (make-hasheqv) 0 0)])
         (thunk))]))

  ; Each lambda becomes a generated class. Its datum is lifted with syntax-local-lift-expression,
  ; and the lifted identifiers wait here until tui-lifts-into adds them to the program.
  (define lifted-classes (box '()))

  (define (the-mctx stx)
    (or (current-mctx) (raise-syntax-error 'tui "Tui expression outside a method body" stx)))

  (define (alloc-local! stx ty) (alloc-local-in! (the-mctx stx) ty))

  (define (new-cell-code ty) (list '(ldc.i4.1) `(newarr ,(tui-type->il ty))))

  (define (alloc-local-in! m ty)
    (define i (length (mctx-locals m)))
    (set-mctx-locals! m (append (mctx-locals m) (list ty)))
    i)

  (define (new-mctx parent) (mctx '() 0 parent '()))

  (define (fresh-label! stx base)
    (define m (the-mctx stx))
    (set-mctx-labels! m (add1 (mctx-labels m)))
    (string->symbol (format "~a~a" base (mctx-labels m))))

  ; The type is also stamped on the form as the 'tui-type syntax property, so it can be read
  ; from an expansion without unpacking the payload.
  (define (il ty instrs)
    (syntax-property #`(tui-il #,(datum->syntax #f (list ty instrs))) 'tui-type ty))

  ; Expands stx to a typed fragment: (values type instructions).
  (define (tc stx [ctx #f])
    (define e (local-expand stx 'expression (list #'tui-il) ctx))
    (syntax-case e ()
      [(head payload)
       (and (identifier? #'head) (free-identifier=? #'head #'tui-il))
       (let ([p (syntax->datum #'payload)])
         (values (or (syntax-property e 'tui-type) (error 'tui "internal: untyped fragment"))
                 (cadr p)))]
      [_ (raise-syntax-error 'tui "not a Tui expression" stx)]))

  ; Expands stx and converts it to type `to`, or raises a type error at stx.
  (define (tc/expect stx to)
    (define-values (ty code) (tc (syntax-property stx 'tui-expected to)))
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
  (struct tui-var (kind index type owner ord)
    #:property prop:procedure
    (λ (self stx)
      (cond
        [(not (identifier? stx)) (raise-syntax-error 'tui "a variable cannot be called" stx)]
        [else (il (tui-var-type self) (load-var self stx))])))

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

  ; A type annotation (an identifier) to a Tui type.
  (define (ann->type stx)
    (or (and (identifier? stx) (resolve-type-name stx))
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

  ;; Tui functions: (define (f [x : T] ...) : R body ...+). The binding of f holds its declared
  ;; signature as syntax; types are resolved when used, after the module's references are set.
  (struct tui-fun (name params ret)
    #:property prop:procedure
    (λ (self stx)
      (syntax-parse stx
        [(f:id arg ...) (compile-tui-call self stx (syntax->list #'(arg ...)))]
        [_ (raise-syntax-error 'tui "functions are not first-class in Tui; call it instead" stx)])))

  (define (make-tui-fun name params ret) (tui-fun name params ret))

  (define (compile-tui-call f stx args)
    (define ptys (map ann->type (tui-fun-params f)))
    (define rty (ann->type (tui-fun-ret f)))
    (unless (= (length args) (length ptys))
      (raise-syntax-error 'tui (format "~a expects ~a arguments, got ~a"
                                       (tui-fun-name f) (length ptys) (length args))
                          stx))
    (il rty (append (append* (map tc/expect args ptys))
                    `((call ,(tui-type->il rty) (Program) ,(tui-fun-name f)
                            ,(map tui-type->il ptys))))))

  (define-syntax-class param
    (pattern [name:id (~datum :) type:id]))

  (define (fun-header def)
    (syntax-parse def
      [(_ (name:id p:param ...) (~datum :) ret:id body ...+)
       (values #'name (syntax->list #'(p.name ...)) (syntax->list #'(p.type ...)) #'ret
               (syntax->list #'(body ...)))]
      [(_ (name:id p:param ...) body ...+)
       (values #'name (syntax->list #'(p.name ...)) (syntax->list #'(p.type ...))
               (datum->syntax #'name 'void) (syntax->list #'(body ...)))]
      [_ (raise-syntax-error 'tui "expected (define (name [param : type] ...) : type body ...+)"
                             def)]))

  ; Compiles one method body. binds are (id kind index type), parent is the enclosing method of
  ; a lambda (#f for a method), and owner is the lambda's class (#f for a method). Returns the
  ; instructions, with the return conversion and ret, and the mctx that holds the locals and the
  ; lambda's env.
  (define (compile-body stx binds body rty parent owner)
    (define m (new-mctx parent))
    (parameterize ([current-mctx m])
      ; A captured argument is copied into a cell, so the lambda and its enclosing method share it.
      (define-values (cell-binds arg-code)
        (for/fold ([bs '()] [code '()] #:result (values (reverse bs) (append* (reverse code))))
                  ([b (in-list binds)])
          (define id (car b))
          (define kind (cadr b))
          (define index (caddr b))
          (define ty (cadddr b))
          (define ord (next-ordinal!))
          (cond
            [(and (eq? kind 'arg) (cell-ordinal? ord))
             (define j (alloc-local-in! m (cell-of ty)))
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
          (if (and (= i n) (not (eq? rty 'void))) (syntax-property b 'tui-expected rty) b)))
      (define-values (bty code) (tc/bind cell-binds #`(tui-begin #,@tail-body)))
      (define ret-code
        (cond
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
      (values (append env-code arg-code code ret-code '((ret))) m)))

  (define (local-il t)
    (if (cell-of? t) `(szarray ,(tui-type->il (cell-of-type t))) (tui-type->il t)))

  (define (locals-decl locals)
    (if (null? locals) '() `((.locals init ,(map local-il locals)))))

  (define (compile-function def)
    (define-values (name pnames ptypes ret body) (fun-header def))
    (define ptys (map ann->type ptypes))
    (define rty (ann->type ret))
    (define-values (code m)
      (run-passes
       (λ ()
         (compile-body def (for/list ([p pnames] [t ptys] [i (in-naturals)]) (list p 'arg i t))
                       body rty #f #f))))
      `(.method (public static) ,(tui-type->il rty) ,(syntax-e name) ,(map tui-type->il ptys)
                (cil managed)
                (,@(locals-decl (mctx-locals m)) ,@code)))

  ; Whether the class type t derives from System.MulticastDelegate.
  (define (delegate-type? t)
    (let loop ([b (base-type t)])
      (and b (or (and (equal? (caddr b) "System") (equal? (cadddr b) "MulticastDelegate"))
                 (loop (base-type b))))))

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

  ; (fn Delegate (param ...) body ...+). The body is the Invoke method of a generated class. The
  ; class has one field per cell that the body reads, and an instance of it is the delegate's
  ; target. dt is the delegate type.
  (define (compile-lambda stx dt pnames body)
    (define-values (ptys rty) (delegate-signature stx dt))
    (unless (= (length pnames) (length ptys))
      (raise-syntax-error 'tui (format "fn has ~a parameters but ~a takes ~a"
                                       (length pnames) (tui-type->string dt) (length ptys))
                          stx))
    (define parent (current-mctx))
    (define cname (string->symbol (symbol->string (gensym 'closure))))
    (set-pass-lambdas! (current-pass) (add1 (pass-lambdas (current-pass))))
    (define-values (code m)
      (compile-body stx (for/list ([p pnames] [t ptys] [i (in-naturals 1)]) (list p 'arg i t))
                    body rty parent cname))
    (if (pass-discovering? (current-pass))
        (il dt '((ldnull)))
        (let ()
          (define env (mctx-env m))
          (define ils (map tui-type->il ptys))
          (define ret-il (tui-type->il rty))
          (define fields
            (for/list ([e (in-list env)] [k (in-naturals)])
              `(.field (public) ,(env-field-type (car e)) ,(env-field k))))
          (define ctor
            '(.method (public specialname rtspecialname) void .ctor () (cil managed)
                      ((ldarg.0) (call void (mscorlib System.Object) .ctor ()) (ret))))
          (define invoke
            `(.method (public) ,ret-il Invoke ,ils (cil managed)
                      (,@(locals-decl (mctx-locals m)) ,@code)))
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
          (il dt `((newobj (,cname) ())
                   ,@env-build
                   (ldftn ,ret-il (,cname) Invoke ,ils)
                   (newobj ,(owner->il dt) (object intptr)))))))


  ; The entry point. With print? the value of the last form, if any, is written to the console.
  (define (compile-main stx forms print?)
    (define-values (code locals)
      (run-passes
       (λ ()
         (parameterize ([current-mctx (new-mctx #f)])
           (define-values (ty code) (tc/bind '() #`(tui-begin #,@forms)))
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
           (values (append code tail '((ret))) (mctx-locals (current-mctx)))))))
    `(.method (public static) void __tui_main () (cil managed)
              ((.entrypoint) ,@(locals-decl locals) ,@code)))

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
         [(free-identifier=? #'h #'tui-define) 'define]
         [(free-identifier=? #'h #'using) 'using]
         [(free-identifier=? #'h #'open) 'open]
         [(ormap (λ (k) (free-identifier=? #'h k))
                 (list #'define-syntax-rule #'define-syntax #'define-syntax-parser
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

  ; (define-syntax f (make-tui-fun ...)) for a Tui function definition.
  (define (fun-declaration def)
    (define-values (name pnames ptypes ret body) (fun-header def))
    #`(define-syntax #,name
        (make-tui-fun '#,name (list #,@(for/list ([t ptypes]) #`(quote-syntax #,t)))
                      (quote-syntax #,ret)))))

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
             (clr-method-ret m))))))

;; Syntax classes that give user macros the Tui types of their arguments. A pattern variable x of
;; class typedExpr has x.Type, the type the checker gives the expression. typedList also has x.Elem,
;; the return type of get_Item, or #f when there is none. typeName matches a type name and has
;; x.Type, the type it names.
; The attribute expressions run at phase 1, so the classes go in a begin-for-syntax beside the helpers.
(begin-for-syntax
  (define-syntax-class typedExpr
    #:attributes (Type)
    (pattern e
      #:attr Type (tui-type-of #'e)))

  (define-syntax-class typedList
    #:attributes (Type Elem)
    (pattern e
      #:attr Type (tui-type-of #'e)
      #:attr Elem (tui-member-return-type (attribute Type) "get_Item")))

  (define-syntax-class typeName
    #:attributes (Type)
    (pattern t:id
      #:attr Type (tui-resolve-type #'t))))

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
     (set-box! lifted-classes '())
     (define defs (of 'define))
     (with-syntax ([prog (datum->syntax stx '%tui-program)]
                   [(decl ...) (map fun-declaration defs)]
                   [(name ...) (for/list ([d defs])
                                 (let-values ([(n p t r b) (fun-header d)]) n))]
                   [(def ...) defs]
                   [(pass ...) (of 'passthrough)]
                   [(stmt ...) (of 'expr)])
       #`(#%module-begin
          (begin-for-syntax (tui-configure! '#,refs '#,opens))
          pass ...
          decl ...
          (define prog
            (tui-lifting
             (make-tui-program (tui-reference-list)
                               (list (cons 'name (tui-method def)) ...)
                               (tui-main stmt ...))))
          (provide (rename-out [prog tui-program]))
          (module+ main (run-tui-program prog))))]))

(define-syntax (tui-reference-list stx)
  #`(quote #,(tui-references)))

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
    [(_ form ...) #`(quote #,(datum->syntax #f (compile-main stx (syntax->list #'(form ...)) #f)))]))

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
    [(_ . id) (raise-syntax-error 'tui "unbound identifier" #'id)]))

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
       [(regexp-match #rx"^(.+)\\.([^.]+)$" name)
        => (λ (m) (compile-static-call stx #'head (cadr m) (caddr m) args explicit))]
       [else (raise-syntax-error 'tui "unbound function" #'head)])]
    [(_ head . _) (raise-syntax-error 'tui "only named functions can be called" stx #'head)]))

(begin-for-syntax
  ; The explicit type arguments of a call, as in Enumerable.Where<int32>.
  (define (explicit-targs head text)
    (for/list ([s (in-list (split-type-args text))])
      (or (with-clr head (λ () (string->tui-type s)))
          (raise-syntax-error 'tui (format "unknown type argument ~a" s) head))))

  (define (compile-static-call stx head type-name method-name args explicit)
    (define-values (tys codes) (tc* args))
    (define ms (with-clr head (λ () (find-static-methods type-name method-name))))
    (unless ms (raise-syntax-error 'tui (format "unknown type ~a" type-name) head))
    (when (null? ms)
      (raise-syntax-error 'tui (format "~a has no public static method ~a" type-name method-name)
                          head))
    (define-values (m convs)
      (with-clr stx (λ () (resolve-overload (list ms) tys
                                            (format "~a.~a" type-name method-name) explicit))))
    (il (clr-method-ret m) (append (args-code codes convs) (list (clr-call-instr 'call m)))))

  ; A value-type receiver is boxed and dispatched through System.Object (ToString and friends).
  (define (compile-instance-call stx head method-name args explicit)
    (when (null? args)
      (raise-syntax-error 'tui (format ".~a needs a receiver" method-name) stx))
    (define-values (rty rcode) (tc (car args)))
    (define-values (tys codes) (tc* (cdr args)))
    (define-values (owner recv-code)
      (cond
        [(value-type? rty) (values 'object (append rcode `((box ,(tui-type->il rty)))))]
        [(eq? rty 'void) (raise-syntax-error 'tui "receiver has type void" (car args))]
        [else (values rty rcode)]))
    (define groups (with-clr head (λ () (find-instance-methods owner method-name))))
    (when (null? groups)
      (raise-syntax-error 'tui (format "~a has no public instance method ~a"
                                       (tui-type->string owner) method-name)
                          head))
    (define-values (m convs)
      (with-clr stx (λ () (resolve-overload groups tys
                                            (format "~a.~a" (tui-type->string owner) method-name)
                                            explicit))))
    (il (clr-method-ret m)
        (append recv-code (args-code codes convs) (list (clr-call-instr 'callvirt m))))))

; (new Type arg ...) calls a public constructor.
(define-tui-form (tui-new stx)
  (syntax-parse stx
    [(_ type:id arg ...)
     (define t (class-of (ann->type #'type)))
     (unless (pair? t) (raise-syntax-error 'tui "new needs a class type" #'type))
     (define-values (tys codes) (tc* (syntax->list #'(arg ...))))
     (define-values (m convs)
       (with-clr stx (λ () (resolve-overload (list (find-constructors t)) tys
                                             (format "new ~a" (tui-type->string t))))))
     (il (ann->type #'type)
         (append (args-code codes convs)
                 `((newobj ,(owner->il t) ,(clr-method-il-params m)))))]))

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

(define-tui-form (tui-begin stx)
  (syntax-parse stx
    [(_) (il 'void '())]
    [(_ e ... last)
     (define code
       (append* (for/list ([e (syntax->list #'(e ...))])
                  (define-values (ty code) (tc e))
                  (if (eq? ty 'void) code (append code '((pop)))))))
     (define-values (ty last-code) (tc #'last))
     (il ty (append code last-code))]))

(define-tui-form (tui-let stx)
  (syntax-parse stx
    [(_ ([x:id e] ...) body ...+)
     (define-values (tys codes) (tc* (syntax->list #'(e ...))))
     (for ([ty tys] [e (syntax->list #'(e ...))])
       (when (eq? ty 'void) (raise-syntax-error 'tui "cannot bind a void value" e)))
     (define xs (syntax->list #'(x ...)))
     (define ords (for/list ([x (in-list xs)]) (next-ordinal!)))
     (define cells? (for/list ([ord (in-list ords)]) (cell-ordinal? ord)))
     (define slots (for/list ([ty tys] [c? cells?]) (alloc-local! stx (if c? (cell-of ty) ty))))
     ; A cell is created each time the let runs, so each run (for example, each loop iteration)
     ; gets a fresh variable.
     (define init
       (append*
        (for/list ([c codes] [slot slots] [ty tys] [c? cells?])
          (if c?
              (append (new-cell-code ty)
                      (list (var-instr 'st 'local slot)
                            (var-instr 'ld 'local slot)
                            '(ldc.i4.0))
                      c
                      (list `(stelem ,(tui-type->il ty))))
              (append c (list (var-instr 'st 'local slot)))))))
     (define-values (ty code)
       (tc/bind (for/list ([x (in-list xs)] [slot (in-list slots)] [t (in-list tys)]
                           [c? (in-list cells?)] [ord (in-list ords)])
                  (list x (if c? 'cell 'local) slot t ord))
                #'(tui-begin body ...)))
     (il ty (append init code))]))

(define-tui-form (tui-set! stx)
  (syntax-parse stx
    [(_ x:id e)
     (define v (syntax-local-value #'x (λ () #f)))
     (unless (tui-var? v) (raise-syntax-error 'tui "not a variable" #'x))
     (il 'void (store-var v #'x (tc/expect #'e (tui-var-type v))))]))

; (fn Delegate (param ...) body ...+), or (fn (param ...) body ...+) when the expected type of the
; form (set by tc/expect as 'tui-expected) is the delegate type.
(define-tui-form (tui-fn stx)
  (syntax-parse stx
    [(_ dt:id (p:id ...) body ...+)
     (compile-lambda stx (ann->type #'dt) (syntax->list #'(p ...)) (syntax->list #'(body ...)))]
    [(_ (p:id ...) body ...+)
     (define expected (syntax-property stx 'tui-expected))
     (unless expected
       (raise-syntax-error 'tui "fn without a delegate type needs an expected delegate type" stx))
     (compile-lambda stx expected (syntax->list #'(p ...)) (syntax->list #'(body ...)))]
    [_ (raise-syntax-error 'tui "expected (fn [Delegate] (param ...) body ...+)" stx)]))

(define-tui-form (tui-if stx)
  (syntax-parse stx
    [(_ c t) #'(tui-if c (tui-begin t (tui-begin)) (tui-begin))]
    [(_ c t e)
     (define ccode (tc/expect #'c 'bool))
     (define-values (tty tcode) (tc #'t))
     (define-values (ety ecode) (tc #'e))
     (define-values (ty tc2 ec2)
       (cond
         [(equal? tty ety) (values tty tcode ecode)]
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
     (define ccode (tc/expect #'c 'bool))
     (define-values (ty bcode) (tc #'(tui-begin body ... (tui-begin))))
     (il 'void (append `((label ,top)) ccode `((brfalse ,end)) bcode `((br ,top) (label ,end))))]))

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
(define-tui-form (tui-= stx)
  (syntax-parse stx
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
