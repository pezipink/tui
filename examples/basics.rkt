#lang tui
; Recursion, loops, locals, and calls into mscorlib. Every .NET call below was resolved at
; expansion time by reading mscorlib.dll with Rangi; type errors are reported by the expander.

(define (fact [n : int]) : int
  (if (< n 2)
      1
      (* n (fact (- n 1)))))

(define (fib [n : int]) : int
  (let ([a 0] [b 1] [i 0])
    (while (< i n)
      (let ([t (+ a b)])
        (set! a b)
        (set! b t)
        (set! i (+ i 1))))
    a))

(define (shout [s : string]) : string
  (+ (.ToUpper s) "!"))

(Console.WriteLine (fact 10))
(Console.WriteLine (fib 30))
(Console.WriteLine (shout "kia ora"))
(Console.WriteLine (Math.Sqrt 2))
(Console.WriteLine (.get_Length "rangi"))
