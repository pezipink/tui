#lang tui
; System.Xml.Linq from Tui. elt-path chains .Element calls, xml-literal builds an element tree at
; expansion time, and type-match and a structural syntax class choose code from the argument types.
(using "System.Xml.Linq")
(using "System.Core")
(require (for-syntax racket/base syntax/parse))
(open System.Xml.Linq)
(open System.Linq)
(open System.Globalization)

; Builds the XElement expression for (name #:attr "value" ... child ...). A child is a nested
; element form or a string of text. Keywords name attributes.
(begin-for-syntax
  (define (xml-node stx)
    (syntax-parse stx
      [(name:id (~seq kw:keyword v:str) ... child ...)
       (define attrs
         (for/list ([k (in-list (syntax->list #'(kw ...)))]
                    [v (in-list (syntax->list #'(v ...)))])
           #`(.Add e (new System.Xml.Linq.XAttribute
                          (System.Xml.Linq.XName.Get #,(keyword->string (syntax-e k))) #,v))))
       (define kids
         (for/list ([c (in-list (syntax->list #'(child ...)))])
           (syntax-parse c
             [s:str #`(.Add e s)]
             [_ #`(.Add e #,(xml-node c))])))
       #`(let ([e (new System.Xml.Linq.XElement
                       (System.Xml.Linq.XName.Get #,(symbol->string (syntax-e #'name))))])
           #,@attrs
           #,@kids
           e)])))

; (elt-path root "a" "b" ...) walks down by name, one .Element call per step.
(define-syntax-parser elt-path
  [(_ root:expr name:str ...)
   (for/fold ([acc #'root]) ([n (in-list (syntax->list #'(name ...)))])
     #`(.Element #,acc (System.Xml.Linq.XName.Get #,n)))])

; (xml-literal (tag #:attr "v" child ...)) builds the element at expansion time, as nested code.
(define-syntax-parser xml-literal
  [(_ x) (xml-node #'x)])

; Anything with a get_Name property of type XName: XElement and XAttribute both qualify.
(define-type-syntax-class named
  #:description "an XElement or XAttribute"
  (has (.get_Name () : System.Xml.Linq.XName)))

(define-syntax-parser name-of
  [(_ x:typedExpr)
   (unless (type-matches? (attribute x.Type) named)
     (raise-syntax-error 'name-of "expected an XElement or XAttribute" this-syntax))
   #'(.ToString (.get_Name x))])

(define-syntax-parser node-kind
  [(_ x:typedExpr)
   (define msg
     (type-match (attribute x.Type)
       [System.Xml.Linq.XElement "element"]
       [System.Xml.Linq.XAttribute "attribute"]
       [else (raise-syntax-error 'node-kind "expected an XElement or XAttribute" this-syntax)]))
   #`#,msg])

(define catalog-text
  "<catalog><book id=\"1\" lang=\"en\"><title>Dune</title><price>9.5</price></book><book id=\"2\" lang=\"mi\"><title>Pukapuka</title><price>12</price></book><book id=\"3\" lang=\"en\"><title>Neuromancer</title><price>7.25</price></book></catalog>")

(let ([root (XElement.Parse catalog-text)])
  (let ([books (Enumerable.ToList (.Elements root))])
    (Console.WriteLine (.get_Count books))
    (Console.WriteLine (.get_Value (elt-path root "book" "title")))
    (let ([total 0.0])
      (.ForEach books (fn ([b : System.Xml.Linq.XElement])
                        (set! total (+ total (Double.Parse (.get_Value (.Element b (XName.Get "price"))) CultureInfo.InvariantCulture)))))
      (Console.WriteLine total))
    (let ([english (Enumerable.Where books (fn ([b : System.Xml.Linq.XElement]) (String.Equals (.get_Value (.Attribute b (XName.Get "lang"))) "en")))])
      (.ForEach (Enumerable.ToList (Enumerable.Select english (fn ([b : System.Xml.Linq.XElement]) (.get_Value (.Element b (XName.Get "title")))))) (fn ([t : string]) (Console.WriteLine t))))
    (let ([shelf (xml-literal (shelf (book #:id "9" "Hyperion") (book #:id "10" "Ubik")))])
      (Console.WriteLine (.ToString shelf)))
    (Console.WriteLine (name-of (elt-path root "book" "title")))
    (Console.WriteLine (name-of (.Attribute (elt-path root "book") (XName.Get "lang"))))
    (Console.WriteLine (node-kind (elt-path root "book")))
    (Console.WriteLine (node-kind (.Attribute (elt-path root "book") (XName.Get "id"))))))
