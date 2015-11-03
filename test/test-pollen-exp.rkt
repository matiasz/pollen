#lang racket/base
(require rackunit racket/runtime-path compiler/find-exe pollen/world racket/system racket/port racket/string)

;; define-runtime-path only allowed at top level
(define-runtime-path tpe-dir "data/pollen-exp/")
(define-runtime-path tpe.rkt "data/pollen-exp/test-pollen-exp.rkt")

;; `find-exe` avoids reliance on $PATH of the host system
(define racket-path (find-exe))

;; parameterize needed to pick up override file
(parameterize ([current-directory tpe-dir]
               [world:current-project-root tpe-dir])
  (when racket-path
    (define (run path)
      (define cmd-string (format "'~a' ~a" racket-path path))
      (with-output-to-string (λ() (system cmd-string))))
    (check-equal? (run tpe.rkt) "fooXbarXzam")))
