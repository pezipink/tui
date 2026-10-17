#lang tui
; (fn Delegate (param ...) body ...+) builds a delegate. Delegate is written out in full, and the
; parameter and return types come from the delegate's Invoke. A generic name with several type
; arguments needs pipes, because the reader splits a symbol at its comma. Captures are in
; captures.rkt.

(define (apply-fn [f : |Func<int32, int32>|] [x : int]) : int
  (.Invoke f x))

(define (square-fn) : |Func<int32, int32>|
  (fn |Func<int32, int32>| (n) (* n n)))

(let ([xs (new System.Collections.Generic.List<int>)])
  (.Add xs 1)
  (.Add xs 2)
  (.Add xs 3)
  (.ForEach xs (fn Action<int32> (x) (Console.WriteLine (* x 10))))
  (Console.WriteLine (.RemoveAll xs (fn Predicate<int32> (x) (= x 2))))
  (Console.WriteLine (.get_Count xs)))

(Console.WriteLine (apply-fn (fn |Func<int32, int32>| (n) (+ n 1)) 41))
(Console.WriteLine (.Invoke (square-fn) 9))
(Console.WriteLine (.Invoke (fn |Func<int32, int32, int32>| (a b) (- a b)) 10 4))
(Console.WriteLine (.Invoke (fn Func<string> () "from fn") ))
(.Invoke (fn Action () (Console.WriteLine "no args")))
