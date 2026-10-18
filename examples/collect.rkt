#lang tui
; (collect T item ...) builds a collection of type T at compile time. The type chooses how to fill it:
; a dictionary takes (key value) entries, and any other type with an Add of its element type takes one
; element per item. When the element type is itself a collection, each item is a group of elements,
; and the macro recurses on the element type to build that inner collection.
(using "System.Core")
(require (for-syntax racket/base syntax/parse))
(provide collect)

; A type that can be filled with Add: an IEnumerable<T> with Add(T) returning void (List) or bool (HashSet).
(define-type-syntax-class collection
  (and (<: (System.Collections.Generic.IEnumerable ?T))
       (or (has (.Add (?T) : void))
           (has (.Add (?T) : bool)))))

(define-syntax-parser collect
  [(_ t:typeName item ...)
   (define ty (attribute t.Type))
   (type-match ty
     [(<: (System.Collections.Generic.IDictionary ?K ?V))
      (define adds
        (for/list ([entry (in-list (syntax->list #'(item ...)))])
          (syntax-parse entry
            [(k:expr v:expr) #'(c.Add k v)]
            [_ (raise-syntax-error 'collect "a dictionary entry is (key value)" entry)])))
      #`(let ([c (new t)]) #,@adds c)]
     [(and (<: (System.Collections.Generic.IEnumerable ?T))
           (or (has (.Add (?T) : void)) (has (.Add (?T) : bool))))
      (define nested? (type-matches? T collection))
      (define elem-id (datum->syntax this-syntax (string->symbol (tui-type->string T))))
      (define adds
        (for/list ([i (in-list (syntax->list #'(item ...)))])
          (cond
            [nested?
             (syntax-parse i
               [(g:expr ...) #`(.Add c (collect #,elem-id g ...))]
               [_ (raise-syntax-error 'collect "each item is a group of elements for the inner collection" i)])]
            [else #`(.Add c #,i)])))
      #`(let ([c (new t)]) #,@adds c)]
     [else
      (raise-syntax-error 'collect
                          (format "~a is not a collection type" (tui-type->string ty))
                          this-syntax)])])

(Console.WriteLine (.get_Count (collect |System.Collections.Generic.List<int32>| 1 2 3)))
(Console.WriteLine (.get_Count (collect |System.Collections.Generic.HashSet<string>| "a" "b" "a")))
(Console.WriteLine (.get_Item (collect |System.Collections.Generic.Dictionary<string,int32>| ("x" 1) ("y" 2)) "y"))
(Console.WriteLine (.get_Count (collect |System.Collections.Generic.List<System.Collections.Generic.HashSet<string>>| ("a" "b") ("b" "c"))))
(Console.WriteLine (.get_Count (.get_Item (collect |System.Collections.Generic.List<System.Collections.Generic.HashSet<string>>| ("a" "b") ("b" "c")) 0)))

(require threading)

(~>>
  (collect |System.Collections.Generic.List<System.Collections.Generic.HashSet<string>>| ("a" "b" "C") ("b" "c"))
  (.get_Item _ 0)
  (.get_Count)
  Console.WriteLine )

