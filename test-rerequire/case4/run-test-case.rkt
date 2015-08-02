#lang sugar/debug racket
(require rackunit racket/file pollen/rerequire)

(define (change-required-file path proc-value [delay 0])
  (display-to-file (format "#lang racket/base
(provide proc)
(define (proc) ~v)" proc-value) path #:exists 'replace)
  (sleep delay) ; to make sure mod time changes so rerequire notices it
  (file-or-directory-modify-seconds path (current-seconds)))

;; create "two.rkt" with `proc` function that evaluates to "foo"
(change-required-file "two.rkt" "foo")
;; even though "two.rkt" is inside a submodule, rerequire will transitively load it the first time
(check-true (and (member (path->complete-path "two.rkt") (dynamic-rerequire "one.rkt")) #t))
;; change "two.rkt"
(change-required-file "two.rkt" "zam" 1)
;; "two.rkt" should appear in next dynamic-rerequire
(check-true (and (member (path->complete-path "two.rkt") (dynamic-rerequire "one.rkt" #:verbosity 'none)) #t)) 
