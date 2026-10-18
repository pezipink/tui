#lang tui
; threading's ~> and ~>> rearrange a pipeline into nested applications while the program compiles,
; so every step is an ordinary Tui call and is type checked like one.
(require threading)

(define (double [n : int]) : int (* n 2))

(Console.WriteLine (~> 3 (+ 4) (* 2)))                  ; (3 + 4) * 2 = 14
(Console.WriteLine (~>> 5 (- 10)))                      ; (- 10 5) = 5
(Console.WriteLine (~> 5 double (+ 1)))                 ; double 5 + 1 = 11
(Console.WriteLine (~> "  tui  " (.Trim) (.ToUpper)))   ; TUI
(Console.WriteLine (~> "pipeline" (.Substring 0 4)))    ; pipe
