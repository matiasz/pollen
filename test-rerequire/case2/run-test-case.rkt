#lang racket
;; case2: render-to-file-if-needed rather than render
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
  (render-to-file-if-needed (->complete-path "one.html.pp"))
  (check-equal? (file->string (->complete-path "one.html")) "first-dr")
  (render-to-file-if-needed (->complete-path "two.html.pp"))
  (check-equal? (file->string (->complete-path "two.html")) "first-dr")
  (make-dr "second-dr")  
  (reset-cache)
  (render-to-file-if-needed (->complete-path "one.html.pp"))
  (check-equal? (file->string (->complete-path "one.html")) "second-dr")
  (render-to-file-if-needed (->complete-path "two.html.pp"))
  (check-equal? (file->string (->complete-path "two.html")) "second-dr"))

