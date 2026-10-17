#lang racket/base
; Writes a Tui module's program to a standalone .exe:
;   racket -l- tui/build examples/basics.rkt build/basics.exe
(require racket/cmdline racket/file "private/runtime.rkt")

(module+ main
  (command-line
   #:program "tui/build"
   #:args (source out)
   (define program (dynamic-require (path->complete-path source) 'tui-program))
   (define-values (dir _name _dir?) (split-path (path->complete-path out)))
   (when (path? dir) (make-directory* dir))
   (tui-write-exe program out)
   (printf "wrote ~a\n" out)))
