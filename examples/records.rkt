#lang tui
; A record is a named collection of fields. Each field gets a constructor parameter, a private
; field, and a get_ and set_ property. Records are declared at module level only.
(require (for-syntax racket/base syntax/parse))
(record Point [x : int32] [y : int32])
(record Person [name : string] [age : int32])
(record Line [start : Point] [finish : Point])

(define (move [p : Point] [dx : int32]) : Point (new Point (+ p.x dx) p.y))

; Syntax classes over the record type. pointType names Point directly. hasXY is structural: any
; type with int32 x and y properties. segment uses Point as the type of a Line's properties.
; xValue binds the type of get_x as ?T.
(define-type-syntax-class pointType
  #:description "a Point"
  Point)

(define-type-syntax-class hasXY
  #:description "a type with int32 x and y properties"
  (has (.get_x () : int32) (.get_y () : int32)))

(define-type-syntax-class segment
  #:description "a Line of two Points"
  (has (.get_start () : Point) (.get_finish () : Point)))

(define-type-syntax-class xValue
  #:description "a type with an x property"
  (has (.get_x () : ?T)))

(define-syntax-parser point-x
  [(_ p:pointType) #'(.get_x p)])

(define-syntax-parser x-type
  [(_ v:xValue) #`#,(tui-type->string (attribute v.T))])

(define-syntax-parser manhattan
  [(_ p:hasXY) #'(let ([v p]) (+ (.get_x v) (.get_y v)))])

(define-syntax-parser describe-shape
  [(_ x:typedExpr)
   (define msg
     (type-match (attribute x.Type)
       [Point "a Point"]
       [Line "a Line"]
       [else "something else"]))
   #`#,msg])

(define-syntax-parser shape-kind
  [(_ x:typedExpr)
   #`#,(cond
         [(type-matches? (attribute x.Type) pointType) "point"]
         [(type-matches? (attribute x.Type) segment) "segment"]
         [(type-matches? (attribute x.Type) hasXY) "has x and y"]
         [else "other"])])

(let ([p (new Point 3 4)])
  (Console.WriteLine p.x)
  (set! p.x 10)
  (Console.WriteLine p.x)
  (p.set_y 7)
  (Console.WriteLine p.y)
  (let ([q (move p 5)])
    (Console.WriteLine q.x)
    (Console.WriteLine q.y)))

(let ([who (new Person "Ada" 36)])
  (Console.WriteLine who.name)
  (Console.WriteLine who.age))

(let ([seg (new Line (new Point 1 2) (new Point 5 6))])
  (let ([end seg.finish])
    (Console.WriteLine end.y)))

(let ([p (new Point 3 4)] [seg (new Line (new Point 1 2) (new Point 5 6))])
  (Console.WriteLine (describe-shape p))
  (Console.WriteLine (describe-shape seg))
  (Console.WriteLine (describe-shape "s"))
  (Console.WriteLine (shape-kind seg.start))
  (Console.WriteLine (shape-kind seg))
  (Console.WriteLine (shape-kind "s"))
  (Console.WriteLine (point-x p))
  (Console.WriteLine (x-type p))
  (Console.WriteLine (manhattan p)))
