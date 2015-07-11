#lang racket
;; case0: require cache deactivated.
(require pollen/render sugar pollen/cache rackunit racket/file pollen/world)

(define (touch ps) (display-to-file (file->string ps) ps #:exists 'replace))
(define dr-path "directory-require.rkt")
(define (make-dr arg)
  (display-to-file (format "#lang racket/base
(module config racket/base
  (provide (all-defined-out))
  (define require-cache-active #f))
(provide do)
(define (do) ~v)" arg) dr-path #:exists 'replace))

(when (file-exists? "pollen.cache") (delete-file "pollen.cache"))
(parameterize ([current-cache (make-cache)]
               [current-output-port (open-output-string)])
  (make-dr "first-dr")
  (reset-cache)
  (check-equal? (render (->complete-path "one.html.pp")) "first-dr")
  (check-true (hash-empty? (current-cache)))
  (check-equal? (render (->complete-path "two.html.pp")) "first-dr")
  (check-true (hash-empty? (current-cache)))
  (make-dr "second-dr")  
  (reset-cache)
  (check-equal? (render (->complete-path "one.html.pp")) "second-dr")
  (check-true (hash-empty? (current-cache)))
  (check-equal? (render (->complete-path "two.html.pp")) "second-dr")
  (check-true (hash-empty? (current-cache))))


