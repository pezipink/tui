#lang tui
; A macro that takes a type name as its argument. (default-of T) expands to the default value of T,
; chosen from T's metadata: a literal for value types and string, or (new T) for a class with a
; public parameterless constructor. Any other type is a syntax error at the use site.
(require (for-syntax racket/base syntax/parse))
(provide default-of type-name-of)

(define-syntax-parser default-of
  [(_ t:typeName)
   (define ty (attribute t.Type))
   (cond
     [(eq? ty 'int32) #'0]
     [(eq? ty 'bool) #'#f]
     [(eq? ty 'float64) #'0.0]
     [(or (eq? ty 'string) (equal? ty (class-of 'string))) #'""]
     [(tui-has-default-ctor? ty) #'(new t)]
     [else (raise-syntax-error
            'default-of
            (format "default-of supports int32, bool, float64, string and classes with a public parameterless constructor, got ~a"
                    (tui-type->string ty))
            this-syntax)])])

; (type-name-of e) expands to the Tui name of e's type, as a string literal. e is type checked but
; not evaluated.
(define-syntax-parser type-name-of
  [(_ e:typedExpr) #`#,(tui-type->string (attribute e.Type))])

(Console.WriteLine (default-of int32))
(Console.WriteLine (default-of bool))
(Console.WriteLine (.get_Length (default-of string)))
(Console.WriteLine (.get_Length (default-of System.String)))
(Console.WriteLine (.get_Length (default-of System.Text.StringBuilder)))
(Console.WriteLine (type-name-of (list-of 1 2)))

; null is a value of a reference type: a bare null takes the type of the parameter it is passed to.
(define (missing [s : string]) : bool (null? s))
(Console.WriteLine (missing null))
(Console.WriteLine (missing "x"))
