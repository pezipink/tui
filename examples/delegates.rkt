#lang tui
; (fn (param ...) body ...+) builds a delegate. The delegate type is often left out: it comes from
; the expected type, the chosen overload, or the body (architecture.md section 10). A parameter
; that nothing else fixes is annotated, as in [x : int]. The explicit form
; (fn Delegate (param ...) body ...+) still works. A generic name with several type arguments needs
; pipes, because the reader splits a symbol at its comma. Captures are in captures.rkt.

(define (apply-fn [f : |Func<int32, int32>|] [x : int]) : int
  (.Invoke f x))

(define (square-fn) : |Func<int32, int32>|
  (fn (n) (* n n)))

(let ([xs (new System.Collections.Generic.List<int>)])
  (.Add xs 1)
  (.Add xs 2)
  (.Add xs 3)
  (.ForEach xs (fn (x) (Console.WriteLine (* x 10))))
  (Console.WriteLine (.RemoveAll xs (fn (x) (= x 2))))
  (Console.WriteLine (.get_Count xs)))

(Console.WriteLine (apply-fn (fn (n) (+ n 1)) 41))
(Console.WriteLine (.Invoke (square-fn) 9))
(Console.WriteLine (.Invoke (fn ([a : int] [b : int]) (- a b)) 10 4))
(Console.WriteLine (.Invoke (fn () "from fn")))
(.Invoke (fn () (Console.WriteLine "no args")))
