#lang tui
; Module-level values: static fields, set during startup in source order.

(define counter 5)
(define doubled (* counter 2))
(define adder (fn ([x : int]) (+ x counter)))

(define (bump) : void (set! counter (+ counter 1)))

(Console.WriteLine counter)
(bump)
(bump)
(Console.WriteLine doubled)
(Console.WriteLine (.Invoke adder 10))
