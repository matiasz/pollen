#lang sugar/debug racket
(require rackunit racket/file racket/format pollen/rerequire sugar pollen/render pollen/cache )

(define (change-required-file path proc-value [delay 0])
  (display-to-file (format "#lang racket/base
(provide color)
(define color ~v)" proc-value) path #:exists 'replace)
  (sleep delay)
  (file-or-directory-modify-seconds path (current-seconds)))

(define (render-contains? path key)
  (dynamic-rerequire "directory-require.rkt") ; <- this line is essential to making the tests work
  (and (regexp-match key (render (->complete-path path))) #t))

(define (eval-render-contains? path key)
   (define eval-result (eval `(begin (local-require pollen/render sugar/coerce)
                                     (render (->complete-path ,path))) (make-base-namespace)))
  (and (regexp-match key eval-result) #t))


(change-required-file "directory-require.rkt" "first")
(check-true (render-contains? "one.txt.pm" "first"))

(change-required-file "directory-require.rkt" "second" 1)
(check-true (render-contains? "one.txt.pm" "second"))

(change-required-file "directory-require.rkt" "first" 1)
(check-true (eval-render-contains? "one.txt.pm" "first"))

(change-required-file "directory-require.rkt" "second" 1)
(check-true (eval-render-contains? "one.txt.pm" "second"))