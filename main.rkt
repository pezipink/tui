#lang racket/base
; Tui: a tiny statically typed .NET language. The macro expander is the compiler. Each Tui form is
; a macro that type checks its subforms by expanding them (local-expand stops at the internal
; tui-il form) and produces a typed fragment of Rangi ilasm. Variables are real Racket bindings in
; internal definition contexts, so hygiene and define-syntax-rule macros work unchanged. Calls to
; .NET are resolved at expansion time by reading the referenced assemblies with Rangi's reader.
; The module's program is kept as ilasm datums, assembled with Rangi when it runs.
(require (for-syntax racket/base racket/list racket/string syntax/parse "private/clr.rkt")
         "private/runtime.rkt")

(provide (rename-out [tui-module-begin #%module-begin]
                     [tui-top-interaction #%top-interaction]
                     [tui-app #%app] [tui-datum #%datum] [tui-top #%top]
                     [tui-define define] [tui-let let] [tui-set! set!] [tui-if if]
                     [tui-while while] [tui-begin begin] [tui-and and] [tui-or or] [tui-not not]
                     [tui-new new]
                     [tui-+ +] [tui-- -] [tui-* *] [tui-/ /] [tui-% %]
                     [tui-< <] [tui-> >] [tui-<= <=] [tui->= >=] [tui-= =])
         ; syntax-rules compiles its patterns at phase 1, so the ellipsis is needed there too
         using open define-syntax-rule ... _ (for-syntax ... _))

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
  ;; Per-method state: local variable types (in slot order) and a label counter.
  (struct mctx ([locals #:mutable] [labels #:mutable]))
  (define current-mctx (make-parameter #f))

  (define (the-mctx stx)
    (or (current-mctx) (raise-syntax-error 'tui "Tui expression outside a method body" stx)))

  (define (alloc-local! stx ty)
    (define m (the-mctx stx))
    (define i (length (mctx-locals m)))
    (set-mctx-locals! m (append (mctx-locals m) (list ty)))
    i)

  (define (fresh-label! stx base)
    (define m (the-mctx stx))
    (set-mctx-labels! m (add1 (mctx-labels m)))
    (string->symbol (format "~a~a" base (mctx-labels m))))

  (define (il ty instrs)
    #`(tui-il #,(datum->syntax #f (list ty instrs))))

  ; Expands stx to a typed fragment: (values type instructions).
  (define (tc stx [ctx #f])
    (define e (local-expand stx 'expression (list #'tui-il) ctx))
    (syntax-case e ()
      [(head payload)
       (and (identifier? #'head) (free-identifier=? #'head #'tui-il))
       (let ([p (syntax->datum #'payload)]) (values (car p) (cadr p)))]
      [_ (raise-syntax-error 'tui "not a Tui expression" stx)]))

  ; Expands stx and converts it to type `to`, or raises a type error at stx.
  (define (tc/expect stx to)
    (define-values (ty code) (tc stx))
    (define c (coercion ty to))
    (unless c
      (raise-syntax-error 'tui (format "type mismatch: expected ~a, got ~a"
                                       (tui-type->string to) (tui-type->string ty))
                          stx))
    (append code (cdr c)))

  (define (tc* stxs)
    (for/lists (tys codes) ([s (in-list stxs)]) (tc s)))

  ; Variables. kind is 'arg or 'local; the binding's value is the transformer itself, so
  ; set! can find the slot with syntax-local-value.
  (struct tui-var (kind index type)
    #:property prop:procedure
    (λ (self stx)
      (if (identifier? stx)
          (il (tui-var-type self) (list (var-instr 'ld (tui-var-kind self) (tui-var-index self))))
          (raise-syntax-error 'tui "a variable cannot be called" stx))))

  (define (make-tui-var kind index type) (tui-var kind index type))

  ; III.3.38 ldarg, III.3.43 ldloc, III.3.61 starg, III.3.63 stloc, short forms where they fit.
  (define (var-instr op kind i)
    (define base (format "~a~a" op (if (eq? kind 'arg) "arg" "loc")))
    (cond
      [(and (< i 4) (not (and (eq? op 'st) (eq? kind 'arg))))
       (list (string->symbol (format "~a.~a" base i)))]
      [(< i 256) (list (string->symbol (string-append base ".s")) i)]
      [else (list (string->symbol base) i)]))

  ; Binds each (id kind index type) in a fresh definition context, then type checks body there.
  (define (tc/bind binds body)
    (define ctx (syntax-local-make-definition-context))
    (for ([b (in-list binds)])
      (syntax-local-bind-syntaxes
       (list (internal-definition-context-introduce ctx (car b) 'add))
       #`(make-tui-var '#,(cadr b) #,(caddr b) '#,(cadddr b))
       ctx))
    (tc (internal-definition-context-introduce ctx body 'add) ctx))

  ; A type annotation (an identifier) to a Tui type.
  (define (ann->type stx)
    (or (and (identifier? stx) (resolve-type-name (syntax-e stx)))
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

  ; Compiles one method body. binds are (id kind index type); returns the body instructions with
  ; the return conversion and ret, plus the locals the body allocated.
  (define (compile-body stx binds body rty)
    (parameterize ([current-mctx (mctx '() 0)])
      (define-values (bty code) (tc/bind binds #`(tui-begin #,@body)))
      (define ret-code
        (cond
          [(eq? rty 'void) (if (eq? bty 'void) '() '((pop)))]
          [(coercion bty rty) => cdr]
          [else (raise-syntax-error 'tui (format "function returns ~a but its body has type ~a"
                                                 (tui-type->string rty) (tui-type->string bty))
                                    stx)]))
      (values (append code ret-code '((ret))) (mctx-locals (current-mctx)))))

  (define (locals-decl locals)
    (if (null? locals) '() `((.locals init ,(map tui-type->il locals)))))

  (define (compile-function def)
    (define-values (name pnames ptypes ret body) (fun-header def))
    (define ptys (map ann->type ptypes))
    (define rty (ann->type ret))
    (define-values (code locals)
      (compile-body def (for/list ([p pnames] [t ptys] [i (in-naturals)]) (list p 'arg i t))
                    body rty))
    `(.method (public static) ,(tui-type->il rty) ,(syntax-e name) ,(map tui-type->il ptys)
              (cil managed)
              (,@(locals-decl locals) ,@code)))

  ; The entry point. With print? the value of the last form, if any, is written to the console.
  (define (compile-main stx forms print?)
    (define-values (code locals)
      (parameterize ([current-mctx (mctx '() 0)])
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
        (values (append code tail '((ret))) (mctx-locals (current-mctx)))))
    `(.method (public static) void __tui_main () (cil managed)
              ((.entrypoint) ,@(locals-decl locals) ,@code)))

  (define (clr-call-instr op m)
    `(,op ,@(if (clr-method-static? m) '() '(instance))
          ,(tui-type->il (clr-method-ret m)) ,(owner->il (clr-method-owner m))
          ,(string->symbol (clr-method-name m)) ,(map tui-type->il (clr-method-params m))))

  ;; Module-level form classification, shared by #%module-begin and #%top-interaction.
  (define (form-kind f)
    (syntax-parse f
      [(h:id . _)
       (cond
         [(free-identifier=? #'h #'tui-define) 'define]
         [(free-identifier=? #'h #'using) 'using]
         [(free-identifier=? #'h #'open) 'open]
         [(free-identifier=? #'h #'define-syntax-rule) 'macro]
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
     (define defs (of 'define))
     (with-syntax ([prog (datum->syntax stx '%tui-program)]
                   [(decl ...) (map fun-declaration defs)]
                   [(name ...) (for/list ([d defs])
                                 (let-values ([(n p t r b) (fun-header d)]) n))]
                   [(def ...) defs]
                   [(macro ...) (of 'macro)]
                   [(stmt ...) (of 'expr)])
       #`(#%module-begin
          (begin-for-syntax (tui-configure! '#,refs '#,opens))
          macro ...
          decl ...
          (define prog
            (make-tui-program (tui-reference-list)
                              (list (cons 'name (tui-method def)) ...)
                              (tui-main stmt ...)))
          (provide (rename-out [prog tui-program]))
          (module+ main (run-tui-program prog))))]))

(define-syntax (tui-reference-list stx)
  #`(quote #,(tui-references)))

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
     (case (form-kind #'form)
       [(define)
        (define-values (name p t r b) (fun-header #'form))
        #`(begin #,(fun-declaration #'form)
                 (tui-program-add-method! #,prog '#,name (tui-method form)))]
       [(using)
        (define p (using-path #'form))
        #`(begin (begin-for-syntax (tui-add-reference! #,p))
                 (tui-program-add-reference! #,prog #,p))]
       [(open) #`(begin-for-syntax (tui-add-open! #,(open-namespace #'form)))]
       [(macro) #'form]
       [else #`(run-tui-main #,prog (tui-main/print form))])]))

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
     (define name (symbol->string (syntax-e #'head)))
     (define args (syntax->list #'(arg ...)))
     (cond
       [(regexp-match #rx"^\\.([^.]+)$" name)
        => (λ (m) (compile-instance-call stx #'head (cadr m) args))]
       [(regexp-match #rx"^(.+)\\.([^.]+)$" name)
        => (λ (m) (compile-static-call stx #'head (cadr m) (caddr m) args))]
       [else (raise-syntax-error 'tui "unbound function" #'head)])]
    [(_ head . _) (raise-syntax-error 'tui "only named functions can be called" stx #'head)]))

(begin-for-syntax
  (define (compile-static-call stx head type-name method-name args)
    (define-values (tys codes) (tc* args))
    (define ms (with-clr head (λ () (find-static-methods type-name method-name))))
    (unless ms (raise-syntax-error 'tui (format "unknown type ~a" type-name) head))
    (when (null? ms)
      (raise-syntax-error 'tui (format "~a has no public static method ~a" type-name method-name)
                          head))
    (define-values (m convs)
      (with-clr stx (λ () (resolve-overload (list ms) tys
                                            (format "~a.~a" type-name method-name)))))
    (il (clr-method-ret m) (append (args-code codes convs) (list (clr-call-instr 'call m)))))

  ; A value-type receiver is boxed and dispatched through System.Object (ToString and friends).
  (define (compile-instance-call stx head method-name args)
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
                                            (format "~a.~a" (tui-type->string owner) method-name)))))
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
     (il (resolve-type-name (syntax-e #'type))
         (append (args-code codes convs)
                 `((newobj ,(owner->il t) ,(map tui-type->il (clr-method-params m))))))]))

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
     (define slots (for/list ([ty tys]) (alloc-local! stx ty)))
     (define init (append* (for/list ([c codes] [i slots]) (append c (list (var-instr 'st 'local i))))))
     (define-values (ty code)
       (tc/bind (for/list ([x (syntax->list #'(x ...))] [i slots] [t tys]) (list x 'local i t))
                #'(tui-begin body ...)))
     (il ty (append init code))]))

(define-tui-form (tui-set! stx)
  (syntax-parse stx
    [(_ x:id e)
     (define v (syntax-local-value #'x (λ () #f)))
     (unless (tui-var? v) (raise-syntax-error 'tui "not a variable" #'x))
     (il 'void (append (tc/expect #'e (tui-var-type v))
                       (list (var-instr 'st (tui-var-kind v) (tui-var-index v)))))]))

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
