#lang tui
; Arrays: literals with array and new-array, aref to read and write elements, and Length. for-each
; loops over arrays, jagged arrays and IEnumerable<T>, and array arguments to overloaded methods.
(using "System.Core")
(open System.Linq)

(define (sum [xs : |int32[]|]) : int32
  (let ([total 0])
    (for-each [x xs] (set! total (+ total x)))
    total))

(define grid : |int32[][]| (array |int32[]| (array int32 1 2 3) (array int32 4 5 6)))

(let ([a (new-array int32 3)])
  (set! (aref a 0) 1)
  (set! (aref a 1) 2)
  (set! (aref a 2) 3)
  (Console.WriteLine (sum a))
  (Console.WriteLine (aref a 1)))

; A nested loop visits every element of the jagged array in order.
(for-each [row grid]
  (for-each [x row]
    (Console.WriteLine x)))

(let ([total 0])
  (for-each [row grid]
    (for-each [x row] (set! total (+ total x))))
  (Console.WriteLine total))

(let ([row (aref grid 1)])
  (Console.WriteLine row.Length)
  (Console.WriteLine (Enumerable.Contains row 5))
  (Console.WriteLine (Enumerable.Contains row 9)))

; An annotated loop variable must match the element type.
(for-each [x : int32 (aref grid 0)]
  (Console.WriteLine (* x 10)))

(for-each [c "ab"]
  (Console.WriteLine c))

(let ([xs (list-of 7 8)])
  (for-each [x xs]
    (Console.WriteLine x)))

(let ([a (array int32 4 5 6)])
  (let ([f (fn () (aref a 2))])
    (Console.WriteLine (f))))
