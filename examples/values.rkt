#lang tui
; Module-level values: static fields, set during startup in source order.

(define counter : int 5)
(define doubled : int (* counter 2))
(define adder : |Func<int32, int32>| (fn (x) (+ x counter)))

(define (bump) : void (set! counter (+ counter 1)))

(Console.WriteLine counter)
(bump)
(bump)
(Console.WriteLine doubled)
(Console.WriteLine (.Invoke adder 10))
