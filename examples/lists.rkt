#lang tui
; (list-of e ...) builds a System.Collections.Generic.List<T>. T is the type of the first element,
; every other element must convert to it, and Add is resolved on the closed List<T>.

(let ([names (list-of "ada" "grace")])
  (.Add names "linus")
  (Console.WriteLine (.get_Count names))
  (Console.WriteLine (.get_Item names 2)))

(let ([xs (list-of 1 2 3)] [i 0] [total 0])
  (while (< i (.get_Count xs))
    (set! total (+ total (.get_Item xs i)))
    (set! i (+ i 1)))
  (Console.WriteLine total))
