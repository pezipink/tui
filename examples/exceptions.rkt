#lang tui
; try/catch/finally, throw and rethrow. A try is a statement. Its value (if it has one) comes from
; the body or from a catch clause, and a finally clause runs however the try is left.

(define (parse [s : string]) : int
  (try
    (System.Int32.Parse s)
    (catch [e : System.FormatException]
      (Console.WriteLine "not a number")
      -1)))

(define (checked-div [a : int] [b : int]) : int
  (if (= b 0)
      (throw (new System.DivideByZeroException "b is zero"))
      (/ a b)))

(define (report [a : int] [b : int]) : void
  (try
    (Console.WriteLine (checked-div a b))
    (catch [e : System.DivideByZeroException]
      (Console.WriteLine (.get_Message e)))
    (finally (Console.WriteLine "checked"))))

(define (show [n : int]) : void
  (try
    (if (< n 0)
        (throw (new System.ArgumentOutOfRangeException "n"))
        (Console.WriteLine n))
    (catch [e : System.ArgumentOutOfRangeException]
      (Console.WriteLine "logging")
      (rethrow))))

(report 6 3)
(report 1 0)
(Console.WriteLine (parse "42"))
(Console.WriteLine (parse "x"))
(show 5)
(try
  (show -1)
  (catch e (Console.WriteLine "outer caught")))
