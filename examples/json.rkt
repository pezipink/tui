#lang tui
; A JSON parser written in Tui. Parsed values live in one flat table of PNode records, because a
; record cannot refer to itself, so children and siblings are linked by index. json-literal turns
; JSON-shaped syntax into JSON text at expansion time, json-get walks a document with a path of
; keys and indices, and json-show chooses how to print a value from its type.
(require (for-syntax racket/base racket/string syntax/parse))
(open System.Text)
(open System.Globalization)

(record PNode [kind : int32] [key : string] [text : string] [num : float64] [first : int32] [next : int32])

(define K-LIT : int32 0)
(define K-NUM : int32 1)
(define K-STR : int32 2)
(define K-ARR : int32 3)
(define K-OBJ : int32 4)
(define K-MEMBER : int32 5)

(define nodes : |System.Collections.Generic.List<PNode>| (new |System.Collections.Generic.List<PNode>|))
(define src : string "")
(define pos : int32 0)

; Tui has no runtime throw, so a parse error prints its message and exits with code 1.
(define (fail [msg : string]) : int32
  (begin (Console.WriteLine msg) (Environment.Exit 1) 0))
(define (cur) : int32 (if (< pos (.get_Length src)) (Convert.ToInt32 (.get_Chars src pos)) 0))
(define (advance) : void (set! pos (+ pos 1)))
(define (put [sb : StringBuilder] [s : string]) : void (.Append sb s))

(define (skip-ws) : void
  (while (or (= (cur) 32) (= (cur) 10) (= (cur) 13) (= (cur) 9)) (advance)))

(define (expect [c : int32]) : void
  (if (not (= (cur) c)) (fail "unexpected character"))
  (advance))

(define (add-node [kind : int32] [key : string] [text : string] [num : float64]) : int32
  (.Add nodes (new PNode kind key text num -1 -1))
  (- (.get_Count nodes) 1))

; prev = -1 means child is the first child of parent.
(define (attach [parent : int32] [prev : int32] [child : int32]) : int32
  (if (= prev -1)
      (let ([p (.get_Item nodes parent)]) (set! p.first child))
      (let ([q (.get_Item nodes prev)]) (set! q.next child)))
  child)

(define (literal [word : string] [text : string]) : int32
  (if (.StartsWith (.Substring src pos) word)
      (let ([at (add-node K-LIT "" text 0.0)])
        (set! pos (+ pos (.get_Length word)))
        at)
      (fail "unknown literal")))

(define (unescape [c : int32]) : int32
  (if (= c 110) 10
  (if (= c 116) 9
  (if (= c 114) 13
  (if (= c 98) 8
  (if (= c 102) 12 c))))))

(define (escape [sb : StringBuilder]) : void
  (if (= (cur) 117)
      (begin (advance)
             (.Append sb (Convert.ToChar (Convert.ToInt32 (.Substring src pos 4) 16)))
             (set! pos (+ pos 4)))
      (begin (.Append sb (Convert.ToChar (unescape (cur)))) (advance))))

(define (parse-string) : string
  (expect 34)
  (let ([sb (new StringBuilder)])
    (while (and (not (= (cur) 34)) (< pos (.get_Length src)))
      (if (= (cur) 92)
          (begin (advance) (escape sb))
          (begin (.Append sb (Convert.ToChar (cur))) (advance))))
    (expect 34)
    (.ToString sb)))

(define (number-char? [c : int32]) : bool
  (or (and (>= c 48) (<= c 57)) (= c 45) (= c 43) (= c 46) (= c 101) (= c 69)))

(define (parse-number) : int32
  (let ([start pos])
    (while (number-char? (cur)) (advance))
    (if (= start pos)
        (fail "unexpected character")
        (add-node K-NUM "" "" (Double.Parse (.Substring src start (- pos start)) CultureInfo.InvariantCulture)))))

(define (parse-value) : int32
  (skip-ws)
  (let ([c (cur)])
    (if (= c 123) (parse-object)
    (if (= c 91) (parse-array)
    (if (= c 34) (add-node K-STR "" (parse-string) 0.0)
    (if (= c 116) (literal "true" "true")
    (if (= c 102) (literal "false" "false")
    (if (= c 110) (literal "null" "null")
        (parse-number)))))))))

(define (array-items [arr : int32] [prev : int32]) : void
  (let ([item (attach arr prev (parse-value))])
    (skip-ws)
    (if (= (cur) 44)
        (begin (advance) (array-items arr item))
        (expect 93))))

(define (parse-array) : int32
  (advance)
  (let ([arr (add-node K-ARR "" "" 0.0)])
    (skip-ws)
    (if (= (cur) 93) (advance) (array-items arr -1))
    arr))

(define (object-members [obj : int32] [prev : int32]) : void
  (skip-ws)
  (let ([m (attach obj prev (add-node K-MEMBER (parse-string) "" 0.0))])
    (skip-ws)
    (expect 58)
    (attach m -1 (parse-value))
    (skip-ws)
    (if (= (cur) 44)
        (begin (advance) (object-members obj m))
        (expect 125))))

(define (parse-object) : int32
  (advance)
  (let ([obj (add-node K-OBJ "" "" 0.0)])
    (skip-ws)
    (if (= (cur) 125) (advance) (object-members obj -1))
    obj))

(define (parse-json [text : string]) : int32
  (set! src text)
  (set! pos 0)
  (let ([root (parse-value)])
    (skip-ws)
    (if (= pos (.get_Length src)) root (fail "trailing characters"))))

(define (kind-of [i : int32]) : int32 (.get_kind (.get_Item nodes i)))

(define (expect-kind [i : int32] [k : int32]) : int32
  (if (= (kind-of i) k) i (fail "wrong kind of value")))

(define (member-named [m : int32] [key : string]) : int32
  (if (= m -1)
      (fail (String.Concat "missing key " key))
      (let ([n (.get_Item nodes m)])
        (if (String.Equals (.get_key n) key)
            (.get_first n)
            (member-named (.get_next n) key)))))

(define (step-key [i : int32] [key : string]) : int32
  (member-named (.get_first (.get_Item nodes (expect-kind i K-OBJ))) key))

(define (element-walk [e : int32] [k : int32]) : int32
  (if (= e -1)
      (fail "index out of range")
      (if (= k 0) e (element-walk (.get_next (.get_Item nodes e)) (- k 1)))))

(define (step-index [i : int32] [k : int32]) : int32
  (element-walk (.get_first (.get_Item nodes (expect-kind i K-ARR))) k))

(define (json-str-of [i : int32]) : string
  (.get_text (.get_Item nodes (expect-kind i K-STR))))

(define (json-num-of [i : int32]) : float64
  (.get_num (.get_Item nodes (expect-kind i K-NUM))))

(define (escape-json [s : string]) : string
  (.Replace (.Replace (.Replace s "\\" "\\\\") "\"" "\\\"") "\n" "\\n"))

(define (dump-seq [i : int32] [sb : StringBuilder]) : void
  (if (not (= i -1))
      (begin
        (dump-node i sb)
        (let ([nx (.get_next (.get_Item nodes i))])
          (if (not (= nx -1))
              (begin (put sb ",") (dump-seq nx sb)))))))

(define (dump-node [i : int32] [sb : StringBuilder]) : void
  (let ([n (.get_Item nodes i)])
    (if (= (.get_kind n) K-LIT) (put sb (.get_text n))
    (if (= (.get_kind n) K-NUM) (put sb (Convert.ToString (.get_num n) CultureInfo.InvariantCulture))
    (if (= (.get_kind n) K-STR) (begin (put sb "\"") (put sb (escape-json (.get_text n))) (put sb "\""))
    (if (= (.get_kind n) K-ARR) (begin (put sb "[") (dump-seq (.get_first n) sb) (put sb "]"))
    (if (= (.get_kind n) K-OBJ) (begin (put sb "{") (dump-seq (.get_first n) sb) (put sb "}"))
        (begin (put sb "\"") (put sb (escape-json (.get_key n))) (put sb "\":") (dump-node (.get_first n) sb)))))))))

(define (dump-json [i : int32]) : void
  (let ([sb (new StringBuilder)])
    (dump-node i sb)
    (Console.WriteLine (.ToString sb))))

; Racket's ~s quoting agrees with JSON for the plain ASCII strings used here.
(begin-for-syntax
  (define (json-of stx)
    (syntax-parse stx
      #:datum-literals (obj arr)
      [s:str (format "~s" (syntax-e #'s))]
      [n:number (number->string (syntax-e #'n))]
      [b:boolean (if (syntax-e #'b) "true" "false")]
      [(obj [k:id v] ...)
       (format "{~a}"
               (string-join
                (for/list ([key (in-list (syntax->list #'(k ...)))]
                           [val (in-list (syntax->list #'(v ...)))])
                  (format "~s:~a" (symbol->string (syntax-e key)) (json-of val)))
                ","))]
      [(arr v ...)
       (format "[~a]" (string-join (for/list ([val (in-list (syntax->list #'(v ...)))]) (json-of val)) ","))])))

; (json-literal (obj [key value] ...)) with values that are strings, numbers, booleans,
; (obj ...), or (arr value ...). The JSON text is built at expansion time.
(define-syntax-parser json-literal
  [(_ v) (datum->syntax this-syntax (json-of #'v))])

; (json-get doc step ...) walks the document. A string step selects an object member, and an
; integer step selects an array element. It returns the index of the node it reaches.
(define-syntax-parser json-get
  [(_ root:expr step ...)
   (for/fold ([acc #'root]) ([s (in-list (syntax->list #'(step ...)))])
     (syntax-parse s
       [k:str #`(step-key #,acc k)]
       [i:nat #`(step-index #,acc i)]))])

; Chooses how to print a value from its static type: strings are written raw, floats in the
; invariant culture.
(define-syntax-parser json-show
  [(_ x:typedExpr)
   (type-match (attribute x.Type)
     [string #'(Console.WriteLine x)]
     [float64 #'(Console.WriteLine (Convert.ToString x CultureInfo.InvariantCulture))]
     [else (raise-syntax-error 'json-show "expected a string or float64" this-syntax)])])

(define sample : string "{\"store\":{\"name\":\"Corner \\\"Books\\\"\",\"open\":true,\"books\":[{\"title\":\"Dune\",\"price\":9.5},{\"title\":\"Neuromancer\",\"price\":7.25}],\"owner\":null},\"tags\":[\"sf\",\"\\u0041bc\"]}")

(let ([doc (parse-json sample)])
  (json-show (json-str-of (json-get doc "store" "name")))
  (json-show (json-num-of (json-get doc "store" "books" 1 "price")))
  (json-show (json-str-of (json-get doc "tags" 1)))
  (dump-json doc))

(let ([made (parse-json (json-literal (obj [title "Dune"] [year 1965] [rating 4.5] [tags (arr "sf" "classic")] [loaned #f])))])
  (dump-json made)
  (json-show (json-str-of (json-get made "tags" 0))))
