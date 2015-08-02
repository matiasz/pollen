#lang sugar/debug racket
(require rackunit racket/file racket/format sugar pollen/render pollen/cache )

(define (change-required-file path proc-value [delay 0])
  (sleep delay)
  (display-to-file (format "#lang racket/base
(provide color)
;(displayln 'dr-loaded)
(define color ~v)" proc-value) path #:exists 'replace)
  (file-or-directory-modify-seconds path (current-seconds)))

(define (render-contains? path key)
  ;(dynamic-rerequire "directory-require.rkt") ; <- this line is essential to making the tests work
  (define render-result (render (->complete-path path)))
  #Rrender-result
  (and (regexp-match key render-result) #t))

;; clue: works ok with preproc, but not markup or markdown
;; clue: has something to do with discovering the template name
(define pp-source "one.txt.pp")
(define pm-source "one.txt.pm")

(change-required-file "directory-require.rkt" "first")
(check-equal? (dynamic-require "directory-require.rkt" 'color) "first")
#;(check-true (render-contains? pp-source "first"))
(check-true (render-contains? pm-source "first"))

(change-required-file "directory-require.rkt" "second" 1)
#;(check-true (render-contains? pp-source "second"))
(check-true (render-contains? pm-source "second"))

