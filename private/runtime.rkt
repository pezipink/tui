#lang racket/base
; Phase 0 half of Tui: a compiled Tui module carries its program as plain Rangi ilasm datums.
; This module turns them into an image with Rangi and runs it.
(require racket/file racket/system file/sha1
         rangi/src/cli/reader rangi/src/ilasm/syntax rangi/src/image)
(provide make-tui-program tui-program? tui-program-references tui-program-methods tui-program-main
         tui-program-add-method! tui-program-add-reference!
         tui-program->il tui-write-exe run-tui-program run-tui-main)

; references: assembly paths. methods: (name . .method datum) pairs in definition order.
; main: the .method datum for the entry point. The REPL adds methods and references.
(struct tui-program (references methods main) #:mutable)

(define (make-tui-program references methods main)
  (tui-program references methods main))

(define (tui-program-add-method! p name m)
  (define others (filter (λ (e) (not (eq? (car e) name))) (tui-program-methods p)))
  (set-tui-program-methods! p (append others (list (cons name m)))))

(define (tui-program-add-reference! p path)
  (unless (member path (tui-program-references p))
    (set-tui-program-references! p (append (tui-program-references p) (list path)))))

; The whole program as Rangi ilasm. Every Tui function is a static method of <Module>, which
; Rangi moves into its generated Program class.
(define (tui-program->il p name [main (tui-program-main p)])
  `(,@(for/list ([r (in-list (tui-program-references p))]) `(.assembly extern ,r ,(extern-body r)))
    (.assembly ,name ())
    (.module ,(string->symbol (format "~a.exe" name)))
    (.class (private auto ansi) <Module>
            (,@(map cdr (tui-program-methods p)) ,main))))

; The version and public key token of a referenced assembly, so the loader can bind it (a
; strong-named framework assembly other than mscorlib does not resolve from version 0 alone).
; II.22.2 Assembly row: hash-alg major minor build revision flags public-key name culture.
; II.6.2.1.3: the token is the last 8 bytes of the SHA-1 of the public key, reversed.
(define (extern-body path)
  (define row (read-row (read-file path) 'assembly 1))
  (define key (vector-ref row 6))
  `(.ver ,(string->symbol (format "~a:~a:~a:~a" (vector-ref row 1) (vector-ref row 2)
                                  (vector-ref row 3) (vector-ref row 4)))
         ,@(if (and key (positive? (bytes-length key)))
               (let ([h (sha1-bytes key)])
                 `(.publickeytoken =
                   ,(for/list ([i (in-range 19 11 -1)])
                      (string->symbol
                       (string-upcase (substring (number->string (+ 256 (bytes-ref h i)) 16) 1))))))
               '())))

; Rangi takes the assembly name from the output file name.
(define (tui-write-exe p path [main (tui-program-main p)])
  (define-values (dir file _) (split-path (path->complete-path path)))
  (define name (string->symbol (path->string (path-replace-extension file #""))))
  (assemble (parse-il (tui-program->il p name main)) path)
  path)

; Assembles into a fresh temp file, runs it with stdout and stderr copied to the current ports,
; then deletes it. Raises if the program exits with a non-zero code.
(define (run-tui-main p main)
  (define exe (make-temporary-file "tui-~a.exe"))
  (dynamic-wind
   void
   (λ ()
     (tui-write-exe p exe main)
     ; the child writes straight to our stdout handle, so anything buffered must go first
     (flush-output (current-output-port))
     (flush-output (current-error-port))
     (define code (system*/exit-code exe))
     (unless (zero? code)
       (error 'tui "program exited with code ~a" code)))
   (λ () (delete-soon exe))))

; Windows (the loader or a virus scanner) can hold an exe briefly after its process exits, so
; deletion is retried for up to a second; a file still locked after that is left in temp.
(define (delete-soon path)
  (let loop ([tries 20])
    (when (file-exists? path)
      (with-handlers ([exn:fail:filesystem? (λ (e) (when (> tries 0) (sleep 0.05) (loop (sub1 tries))))])
        (delete-file path)))))

(define (run-tui-program p) (run-tui-main p (tui-program-main p)))
