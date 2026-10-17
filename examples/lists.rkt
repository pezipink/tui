#lang tui
; (list-of e ...) builds a System.Collections.Generic.List<T>. T is the type of the first element,
; every other element must convert to it, and Add is resolved on the closed List<T>.
(require (for-syntax racket/base syntax/parse))
(provide sum-of)

; (sum-of xs) type checks xs, then reads the element type from the metadata of its closed List<T>
; (the return type of get_Item). The element type picks the starting value and the expansion
; reuses Tui's + for the fold, which joins strings and adds ints.
(define-syntax-parser sum-of
  [(_ xs:expr)
   (define elem (tui-member-return-type (tui-type-of #'xs) "get_Item"))
   (define zero
     (case elem
       [(int32) #'0]
       [(string) #'""]
       [else (raise-syntax-error
              'sum-of
              (format "sum-of needs a list of int32 or string elements, got ~a"
                      (if elem (tui-type->string elem) "a non-list"))
              this-syntax)]))
   #`(let ([src xs] [i 0] [total #,zero])
       (while (< i (.get_Count src))
         (set! total (+ total (.get_Item src i)))
         (set! i (+ i 1)))
       total)])

(let ([names (list-of "ada" "grace")])
  (.Add names "linus")
  (Console.WriteLine (.get_Count names))
  (Console.WriteLine (.get_Item names 2)))

(let ([xs (list-of 1 2 3)] [i 0] [total 0])
  (while (< i (.get_Count xs))
    (set! total (+ total (.get_Item xs i)))
    (set! i (+ i 1)))
  (Console.WriteLine total))

(let ([words (list-of "ada" "grace" "linus")])
  (Console.WriteLine (sum-of words)))
(Console.WriteLine (sum-of (list-of 1 2 3)))
