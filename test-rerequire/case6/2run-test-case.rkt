#lang sugar/debug racket
(require rackunit   pollen/world)

(define (change-required-file path proc-value [delay 0])
  (sleep delay)
  (display-to-file (format "#lang racket/base
(provide color)
;(displayln 'dr-loaded)
(define color ~v)" proc-value) path #:exists 'replace)
  (file-or-directory-modify-seconds path (current-seconds)))


;; clue: when cache hits this expression, it fouls up dyn-require for dr.rkt
;; because it precaches the file or something...
(world:current-cache-dir-name)



(change-required-file "directory-require.rkt" "first")
(check-equal? (dynamic-require "directory-require.rkt" 'color) "first")
(change-required-file "directory-require.rkt" "second")
