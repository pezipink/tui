#lang tui
; Phase 1 macros that read CLR metadata and the type checker while the program is compiled. None of
; them run at runtime: each one expands to a literal, a list, or a boolean.
(require (for-syntax racket/base syntax/parse))
(provide static-overloads instance-overloads implements? member-names squares-upto)

; (static-overloads Console WriteLine) is the number of public static overloads of a type named in
; the source, read from its metadata. The type name is not checked by the type checker.
(define-syntax-parser static-overloads
  [(_ t:id m:id)
   #`#,(tui-static-overloads (symbol->string (syntax-e #'t)) (symbol->string (syntax-e #'m)))])

; (instance-overloads T name) counts the instance overloads of name on T and its base types.
(define-syntax-parser instance-overloads
  [(_ t:typeName m:id)
   #`#,(tui-instance-overloads (attribute t.Type) (symbol->string (syntax-e #'m)))])

; (implements? T I) is #t when I is a base class or interface of T, following the closed generic
; types in the metadata. Both arguments are type names.
(define-syntax-parser implements?
  [(_ t:typeName i:typeName)
   (if (member (attribute i.Type) (supertypes (class-of (attribute t.Type))))
       #'#t
       #'#f)])

; (member-names T) is a List<string> literal of the public instance method names of T, sorted.
(define-syntax-parser member-names
  [(_ t:typeName)
   (define names (tui-member-names (attribute t.Type)))
   (when (null? names)
     (raise-syntax-error 'member-names "type has no public instance methods" this-syntax))
   #`(list-of #,@names)])

; (squares-upto n) expands to a list-of literal of the squares 1..n, computed by Racket while the
; program compiles. n must be a literal integer.
(define-syntax-parser squares-upto
  [(_ n:nat)
   (define k (syntax-e #'n))
   #`(list-of #,@(for/list ([i (in-range 1 (add1 k))]) (* i i)))])

(Console.WriteLine (static-overloads Console WriteLine))
(Console.WriteLine (instance-overloads System.Text.StringBuilder Append))
(Console.WriteLine (implements? |System.Collections.Generic.List<int32>| |System.Collections.Generic.IEnumerable<int32>|))
(Console.WriteLine (implements? |System.Collections.Generic.List<int32>| |System.Collections.Generic.IList<string>|))
(Console.WriteLine (implements? string System.IComparable))
(Console.WriteLine (implements? string System.Collections.Generic.IEnumerable<int32>))
(let ([names (member-names System.Object)])
  (Console.WriteLine (.get_Count names))
  (Console.WriteLine (.get_Item names 0)))
(let ([sq (squares-upto 5)] [i 0] [total 0])
  (while (< i (.get_Count sq))
    (set! total (+ total (.get_Item sq i)))
    (set! i (+ i 1)))
  (Console.WriteLine total))
