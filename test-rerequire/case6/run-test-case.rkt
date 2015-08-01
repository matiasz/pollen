#lang sugar/debug racket
(require rackunit racket/file pollen/rerequire sugar pollen/render pollen/cache )

(define (change-required-file path proc-value [delay 0])
  (display-to-file (format "#lang racket/base
(provide color)
(define color ~v)" proc-value) path #:exists 'replace)
  (sleep delay)
  (file-or-directory-modify-seconds path (current-seconds)))

(define (render-contains? path key)
  (and (regexp-match key (render (->complete-path path))) #t))


(change-required-file "directory-require.rkt" "first")

(check-true (render-contains? "one.txt.pm" "first"))

(change-required-file "directory-require.rkt" "second" 1)

(check-true (render-contains? "one.txt.pm" "second"))