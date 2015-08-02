#lang racket
;; case1: require cache active
(require pollen/render sugar pollen/cache rackunit racket/file)

(define (touch ps) (display-to-file (file->string ps) ps #:exists 'replace))
(define dr-path "directory-require.rkt")
(define (make-dr arg)
  (display-to-file (format "#lang racket/base
(provide do)
(define (do) ~v)" arg) dr-path #:exists 'replace))


(when (directory-exists? "pollen-cache")
  (void (system (format "rm -rf '~a'" (->complete-path "pollen-cache")))))
(parameterize ([current-output-port (open-output-string)])
  (make-dr "first-dr")
  (reset-cache)
  (check-equal? (render (->complete-path "one.html.pp")) "first-dr")
  (check-equal? (render (->complete-path "two.html.pp")) "first-dr")
  (make-dr "second-dr")  
  (reset-cache)
  (check-equal? (render (->complete-path "one.html.pp")) "second-dr")
  (check-equal? (render (->complete-path "two.html.pp")) "second-dr"))

