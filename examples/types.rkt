#lang tui
; A macro that takes a type name as its argument. (default-of T) expands to the default value of T,
; chosen from T's metadata: a literal for value types and string, or (new T) for a class with a
; public parameterless constructor. Any other type is a syntax error at the use site.
(require (for-syntax racket/base syntax/parse))
(provide default-of)

(define-syntax-parser default-of
  [(_ t:id)
   (define ty (tui-resolve-type #'t))
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

(Console.WriteLine (default-of int32))
(Console.WriteLine (default-of bool))
(Console.WriteLine (.get_Length (default-of string)))
(Console.WriteLine (.get_Length (default-of System.String)))
(Console.WriteLine (.get_Length (default-of System.Text.StringBuilder)))
