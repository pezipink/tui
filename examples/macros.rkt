#lang tui
; Tui forms are ordinary Racket macros, so a program can grow its own syntax with
; define-syntax-rule. The expansions are type checked like hand-written code.
; (using "System") adds System.dll; its signatures refer to mscorlib types through TypeRefs,
; which the type checker resolves across the referenced assemblies.
(using "System")
(open System.Text)

(define-syntax-rule (println e) (Console.WriteLine e))
(define-syntax-rule (unless c body ...) (if c (begin) (begin body ...)))
(define-syntax-rule (for ([i from to]) body ...)
  (let ([i from])
    (while (< i to)
      body ...
      (set! i (+ i 1)))))

(define (fizzbuzz [n : int]) : string
  (if (= (% n 15) 0) "FizzBuzz"
      (if (= (% n 3) 0) "Fizz"
          (if (= (% n 5) 0) "Buzz"
              (.ToString n)))))

(define (repeat [s : string] [n : int]) : string
  (let ([sb (new StringBuilder)])
    (for ([i 0 n]) (.Append sb s))
    (.ToString sb)))

(for ([i 1 16]) (println (fizzbuzz i)))
(println (repeat "ab" 3))
(unless (= 1 2) (println "one is not two"))
(let ([u (new Uri "https://github.com/pezipink/rangi")])
  (println (.get_Host u))
  (println (.get_AbsolutePath u)))
