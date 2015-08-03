#lang racket
(require rackunit)
(provide (all-defined-out))

(define (change-required-file path proc-value [delay 0])
  (sleep delay)
  (display-to-file (format "#lang racket/base
(provide color)
(define color ~v)" proc-value) path #:exists 'replace))


;; clue: when cache hits this expression, it fouls up dyn-require for dr.rkt
;; because it precaches the file or something...
;(world:current-cache-dir-name)


;; clue: dynamic-require does not support reloading.
;; theory: sometimes dynamic-require looks like it's supporting reloading,
;; but in general it does not.

(change-required-file "directory-require.rkt" "first" 1)
(void (dynamic-require "directory-require.rkt" 'color))
(change-required-file "directory-require.rkt" "second" 1)
;; clue: here, dyn-req will not re-instantiate the module in current namespace, so value does not change
(check-equal? (dynamic-require "directory-require.rkt" 'color) "first")
;; clue: but in a new namespace, dyn-req re-instantiates, refreshing the value..
(parameterize ([current-namespace (make-base-namespace)])
(check-equal? (dynamic-require "directory-require.rkt" 'color) "second"))
