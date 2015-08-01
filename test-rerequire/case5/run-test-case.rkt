#lang sugar/debug racket
(require rackunit racket/file pollen/rerequire sugar  pollen/world )

(define (change-required-file path proc-value [delay 0])
  (display-to-file (format "#lang racket/base
(provide color)
(define color ~v)" proc-value) path #:exists 'replace)
  (sleep delay)
  (file-or-directory-modify-seconds path (current-seconds)))

(define (dr-contains? path target-path)
  (and (member (->complete-path target-path) (dynamic-rerequire path)) #t))

;; this test only works if pollen/world has no recursive references
;; e.g., dereferences of settable values on the right side of other values.

(check-true (dr-contains? "one-df.rkt" "directory-foobar.rkt"))
(check-true (dr-contains? "one-dr.rkt" "directory-require.rkt"))
