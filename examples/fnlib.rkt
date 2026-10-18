#lang tui
; A small library of Func<int32, int32> helpers. compose is a phase-1 macro: it type checks both of
; its arguments with typedExpr, rejects anything that is not a Func<int32, int32>, and expands to a
; new lambda that captures the two delegates.
(require (for-syntax racket/base syntax/parse))
(provide compose square)

(define-syntax-parser compose
  [(_ f:typedExpr g:typedExpr)
   (define want (tui-resolve-type #'|System.Func<int32, int32>|))
   (for ([stx (list #'f #'g)] [t (list (attribute f.Type) (attribute g.Type))])
     (unless (equal? t want)
       (raise-syntax-error 'compose
                           (format "compose needs Func<int32, int32> arguments, got ~a"
                                   (tui-type->string t))
                           stx)))
   #'(let ([f1 f] [g1 g])
       (fn ([x : int]) (.Invoke f1 (.Invoke g1 x))))])

; square is an identifier macro, so each use expands to a fresh lambda.
(define-syntax-parser square
  [_:id #'(fn ([n : int]) (* n n))])
