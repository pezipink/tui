#lang tui
; LINQ over System.Core. Lambdas are first class here: they are stored with let and in a List,
; passed to LINQ as arguments, and composed by the fnlib library macro. The generic LINQ methods
; take their type arguments from the arguments and from the bare fn bodies.
(require "fnlib.rkt")
(using "System.Core")
(open System.Linq)

(let ([xs (new |System.Collections.Generic.List<int32>|)]
      ; a lambda stored with let, used as an argument below
      [even? (fn ([x : int]) (= (% x 2) 0))])
  (.Add xs 1) (.Add xs 2) (.Add xs 3) (.Add xs 4) (.Add xs 5) (.Add xs 6)

  (let ([evens (Enumerable.Where xs even?)])
    ; the library's square, passed to Select
    (let ([squares (Enumerable.Select evens square)])
      (Console.WriteLine (Enumerable.Sum squares))))          ; 4 + 16 + 36 = 56

  ; composition: square of square, summed over the whole list
  (Console.WriteLine
   (Enumerable.Sum (Enumerable.Select xs (compose square square))))   ; 2275

  ; delegates stored as data in a List, then applied in order by a fold
  (let ([fs (new |System.Collections.Generic.List<Func<int32, int32>>|)])
    (.Add fs square)
    (.Add fs (fn (n) (+ n 1)))
    (.Add fs (fn (n) (* n 10)))
    (let ([i 0 ] [acc 2])
      (while (< i (.get_Count fs))
        (set! acc (.Invoke (.get_Item fs i) acc))
        (set! i (+ i 1)))
      (Console.WriteLine acc)))                               ; ((2*2)+1)*10 = 50

  (Console.WriteLine (Enumerable.Count (Enumerable.Where xs even?)))  ; 3
  (Console.WriteLine (Enumerable.Max xs)))                    ; 6
