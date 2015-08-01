#lang sugar/debug racket
(require rackunit racket/file pollen/rerequire sugar pollen/render pollen/cache pollen/world)

(define (change-required-file path proc-value)
  (display-to-file (format "#lang racket/base
(provide color)
(define color ~v)" proc-value) path #:exists 'replace)
  (sleep 1) ; to make sure mod time changes so rerequire notices it
  (file-or-directory-modify-seconds path (current-seconds)))

(define (contains? string key)
  (and (regexp-match key string) #t))

(world:current-project-root (current-directory))
(reset-cache)

(change-required-file "directory-require.rkt" "orange")
(check-true ((render (->complete-path "markup.html.pm")) . contains? . "orange"))

(change-required-file "directory-require.rkt" "purple")
;; this will fail
(check-true ((render (->complete-path "markup.html.pm")) . contains? . "purple"))
