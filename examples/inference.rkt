#lang tui
; Return types are inferred from the last expression of a function body, and the types of module
; values from their expression, so the annotation can be left out.

(define (square [x : int]) (* x x))
(define (sum-of-squares [a : int] [b : int]) (+ (square a) (square b)))

; a function can call one that is defined after it
(define (shifted [x : int]) (plus-one x))
(define (plus-one [x : int]) (+ x 1))

(define (show [x : int]) (Console.WriteLine x))

(define total (sum-of-squares 3 4))
(define doubled (* total 2))

(show total)
(show doubled)
(show (shifted 41))
