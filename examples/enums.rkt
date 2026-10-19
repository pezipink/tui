#lang tui
; Enum members and other static fields are read by their qualified name, with no call. An enum can
; be passed around, stored in a global or a list, and compared with =. Tui cannot define enums.

(define (weekend? [d : System.DayOfWeek]) : bool
  (if (= d System.DayOfWeek.Saturday) #t (= d System.DayOfWeek.Sunday)))

(define (kind [d : System.DayOfWeek]) : string
  (if (weekend? d) "weekend" "weekday"))

(define today System.DayOfWeek.Friday)

(Console.WriteLine System.DayOfWeek.Monday)
(Console.WriteLine (kind today))
(Console.WriteLine (kind System.DayOfWeek.Sunday))
(Console.WriteLine (= today System.DayOfWeek.Friday))

(let ([days (list-of System.DayOfWeek.Monday System.DayOfWeek.Saturday)])
  (for-each [d days]
    (Console.WriteLine (kind d))))

(Console.WriteLine Int32.MaxValue)
(Console.WriteLine System.Globalization.NumberStyles.Integer)
(Console.WriteLine Math.PI)
