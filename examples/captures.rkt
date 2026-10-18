#lang tui
; A fn captures the locals and arguments of the enclosing method that it uses. A captured variable
; is shared: the lambda and the method see the same variable. Each run of a let makes a new one.

(define (adder [k : int]) : |Func<int32, int32>|
  (fn (n) (+ n k)))

(let ([total 0])
  (let ([bump (fn ([x : int]) (set! total (+ total x)))])
    (.Invoke bump 5)
    (.Invoke bump 7))
  (Console.WriteLine total))

(Console.WriteLine (.Invoke (adder 10) 5))

(let ([base 100])
  (Console.WriteLine (.Invoke (fn () (.Invoke (fn () (+ base 1)))))))

(Console.WriteLine (.Invoke (fn ([n : int]) (.Invoke (fn () (* n 2)))) 21))

(let ([name "tui"])
  (Console.WriteLine (.Invoke (fn () name))))

(let ([fs (new System.Collections.Generic.List<Func<int32>>)]
      [i 0])
  (while (< i 3)
    (let ([j i])
      (.Add fs (fn () (* j 10))))
    (set! i (+ i 1)))
  (Console.WriteLine (.Invoke (.get_Item fs 0)))
  (Console.WriteLine (.Invoke (.get_Item fs 2))))
