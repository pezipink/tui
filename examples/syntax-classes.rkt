#lang tui
; Phase 1 definitions: syntax classes, simple macros and phase-1 helpers. They are emitted as
; written, so they run while the program compiles.
(require (for-syntax racket/base syntax/parse))

; A syntax class used by a macro must be defined at phase 1, so it goes in begin-for-syntax.
(begin-for-syntax
  (define (sum-of stxs) (apply + (map syntax-e stxs)))
  (define-syntax-class small-int
    (pattern n:nat #:when (< (syntax-e #'n) 100))))

(define-simple-macro (twice x:small-int) (+ x x))

(define-for-syntax (squares-of xs) (map (λ (x) (* x x)) xs))

(define-syntax-parser sum-literals
  [(_ n:nat ...) #`#,(sum-of (attribute n))])

(define-syntax-parser squares-of-literals
  [(_ n:nat ...) #`(list-of #,@(squares-of (map syntax-e (attribute n))))])

(Console.WriteLine (twice 21))
(Console.WriteLine (sum-literals 1 2 3 4))
(let ([sq (squares-of-literals 2 3 4)] [i 0] [total 0])
  (while (< i (.get_Count sq))
    (set! total (+ total (.get_Item sq i)))
    (set! i (+ i 1)))
  (Console.WriteLine total))
