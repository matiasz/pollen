#lang sugar/debug racket
(require rackunit racket/file racket/format sugar pollen/render pollen/cache racket/rerequire)

(define (change-required-file path proc-value [delay 0])
  (sleep delay)
  (display-to-file (format "#lang racket/base
(provide color)
(define color ~v)" proc-value) path #:exists 'replace))

(define (render-contains? path key)
  (define render-result (render (->complete-path path)))
  (and (regexp-match key render-result) #t))


(define (force-dynamic-require mod sym)
  (parameterize ([current-namespace (make-base-namespace)])
    (dynamic-require mod sym)))

;; clue: works ok with preproc, but not markup or markdown
;; clue: has something to do with discovering the template name
(define pp-source "one.txt.pp")
(define pm-source "one.txt.pm")

(change-required-file "directory-require.rkt" "first")
;(check-equal? (force-dynamic-require "directory-require.rkt" 'color) "first")
(check-true (render-contains? pp-source "first"))
(check-true (render-contains? pm-source "first"))

(change-required-file "directory-require.rkt" "second" 1)
;(check-equal? (force-dynamic-require "directory-require.rkt" 'color) "second")
(check-true (render-contains? pp-source "second"))
(check-true (render-contains? pm-source "second"))

