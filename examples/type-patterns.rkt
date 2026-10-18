#lang tui
; Type patterns in macros: generic binders, nested and named patterns, members (including
; method type parameters and static members), and the combinators. The grammar is in
; docs/architecture.md, section 12.1.
(require (for-syntax racket/base syntax/parse))

; Generic binders. ?K and ?V are the closed type arguments of the dictionary.
(define-type-syntax-class dictLike
  #:description "an IDictionary<K, V>"
  (<: (System.Collections.Generic.IDictionary ?K ?V)))

(define-syntax-parser dict-types
  [(_ d:dictLike)
   #`#,(format "~a => ~a" (tui-type->string (attribute d.K)) (tui-type->string (attribute d.V)))])

; A named pattern inside another one. The binders of dictLike are not exposed on countedDict.
(define-type-syntax-class countedDict
  #:description "an IDictionary with a Count"
  (and dictLike (has (.get_Count () : int32))))

(define-syntax-parser counted?
  [(_ x:typedExpr)
   #`#,(if (type-matches? (attribute x.Type) countedDict) "counted" "uncounted")])

; The binders are shared across the whole pattern: T is the element type of the IEnumerable and
; the parameter type of Add.
(define-type-syntax-class growableSeq
  #:description "an IEnumerable<T> with Add(T) : void"
  (and (<: (System.Collections.Generic.IEnumerable ?T))
       (has (.Add (?T) : void))))

(define-syntax-parser add-twice
  [(_ c:growableSeq v:expr)
   #'(begin (.Add c v) (.Add c v))])

; A method type parameter (?R) is solved against the member's signature but is not an attribute.
; Only ?T, the class's own argument, is exposed.
(define-type-syntax-class convertible
  #:description "a List<T> with ConvertAll<R>(Converter<T, R>) : List<R>"
  (has (.ConvertAll (?R) ((System.Converter ?T ?R)) : (System.Collections.Generic.List ?R))))

(define-syntax-parser element-type-of
  [(_ c:convertible) #`#,(tui-type->string (attribute c.T))])

; Nested <: and a binder inside a generic argument. Generic arguments match exactly (invariant),
; so the inner position uses <: to accept the covariant IEnumerable<out T> of List<int32>.
(define-type-syntax-class nestedSeq
  #:description "a sequence of sequences of ?X"
  (<: (System.Collections.Generic.IEnumerable (<: (System.Collections.Generic.IEnumerable ?X)))))

(define-syntax-parser inner-type
  [(_ x:nestedSeq) #`#,(tui-type->string (attribute x.X))])

; Static members: a type with a static IsNullOrEmpty(string) : bool.
(define-type-syntax-class stringLike
  #:description "a type with static IsNullOrEmpty(string) : bool"
  (has (static .IsNullOrEmpty (string) : bool)))

(define-syntax-parser empty-check
  [(_ t:stringLike s:expr) #'(System.String.IsNullOrEmpty s)])

; Combinators: or, not, ? with a predicate, and exactly with a phase-1 value.
(define-type-syntax-class scalar
  #:description "a string, int32, int64 or bool"
  (or string int32 int64 bool))

(define-type-syntax-class numeric
  #:description "a numeric type"
  (? (lambda (t) (and (memq t '(int32 int64 float64)) #t))))

(define-for-syntax the-int-type 'int32)

(define-type-syntax-class exactlyInt32
  (exactly the-int-type))

(define-syntax-parser classify
  [(_ x:typedExpr)
   #`#,(cond
         [(type-matches? (attribute x.Type) numeric) "numeric"]
         [(type-matches? (attribute x.Type) scalar) "scalar"]
         [else "other"])])

(define-syntax-parser exactly-int
  [(_ x:typedExpr)
   #`#,(if (type-matches? (attribute x.Type) exactlyInt32) "exactly int32" "not int32")])

; type-match with nested open, binders, named patterns, nested generics and an else clause. The
; szarray clause is compiled but not exercised here: Tui has no array-construction form yet.
(define-syntax-parser describe
  [(_ x:typedExpr)
   (define msg
     (type-match (attribute x.Type)
       [(System.Collections.Generic.Dictionary (open System.Collections.Generic.List) ?V)
        (format "a dictionary keyed by a list, with ~a values" (tui-type->string V))]
       [(System.Collections.Generic.List (System.Collections.Generic.List ?X))
        (format "a list of lists of ~a" (tui-type->string X))]
       [(szarray ?E) (format "an array of ~a" (tui-type->string E))]
       [(<: (System.Collections.Generic.IEnumerable (or string int32 int64 bool)))
        "a sequence of scalars"]
       [scalar "a scalar"]
       [else "something else"]))
   #`#,msg])

; type-match as a code generator: each clause returns the syntax to splice in.
(define-syntax-parser zero-of
  [(_ x:typedExpr)
   (type-match (attribute x.Type)
     [int32 #'0]
     [string #'""]
     [bool #'#f]
     [else #'"none"])])

(define-syntax-parser pick-nested
  [(_ x:typedExpr)
   (type-match (attribute x.Type)
     [(System.Collections.Generic.Dictionary ?K (System.Collections.Generic.List ?V))
      #`#,(format "dict ~a => list of ~a" (tui-type->string K) (tui-type->string V))]
     [else #'"unsupported"])])

(let ([xs (new |System.Collections.Generic.List<int32>|)])
  (add-twice xs 7)
  (Console.WriteLine (.get_Count xs))
  (Console.WriteLine (.get_Item xs 1)))

(Console.WriteLine (dict-types (new |System.Collections.Generic.Dictionary<string,int32>|)))
(Console.WriteLine (counted? (new |System.Collections.Generic.Dictionary<string,int32>|)))
(Console.WriteLine (counted? (new |System.Collections.Generic.List<int32>|)))
(Console.WriteLine (element-type-of (new |System.Collections.Generic.List<string>|)))
(Console.WriteLine (inner-type (new |System.Collections.Generic.List<System.Collections.Generic.List<int32>>|)))
(Console.WriteLine (empty-check "x" ""))
(Console.WriteLine (classify 42))
(Console.WriteLine (classify "s"))
(Console.WriteLine (classify (.Substring "ab" 0)))
(Console.WriteLine (exactly-int 1))
(Console.WriteLine (exactly-int "s"))
(Console.WriteLine (describe (new |System.Collections.Generic.Dictionary<string,int32>|)))
(Console.WriteLine (describe (new |System.Collections.Generic.List<System.Collections.Generic.List<int32>>|)))
(Console.WriteLine (describe (.Substring "ab" 0)))
(Console.WriteLine (describe (new |System.Collections.Generic.List<int32>|)))
(Console.WriteLine (describe (.Contains "abc" "b")))
(Console.WriteLine (.get_Length (zero-of "x")))
(Console.WriteLine (zero-of 5))
(Console.WriteLine (zero-of (.Contains "a" "b")))
(Console.WriteLine (pick-nested (new |System.Collections.Generic.Dictionary<string,System.Collections.Generic.List<int32>>|)))
(Console.WriteLine (pick-nested 3))
