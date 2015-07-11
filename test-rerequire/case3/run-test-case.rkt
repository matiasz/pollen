#lang racket
;; case3: cross-referenced pages
(require pollen/render sugar pollen/cache rackunit racket/file pollen/world racket/serialize)

(define (touch ps) (display-to-file (file->string ps) ps #:exists 'replace))
(define dr-path "directory-require.rkt")
(define (make-dr arg)
  (display-to-file (format "#lang racket/base
(provide do)
(define (do) ~v)" arg) dr-path #:exists 'replace))

(current-output-port (open-output-string))


(current-cache (make-hash))
(make-dr "first-dr")
(render-to-file (->complete-path "one.html.pm"))
(report (hash-ref (hash-ref (current-cache) (->complete-path "one.html.pm")) 'doc) 'c)
(check-false (regexp-match #rx"second-dr" (file->string "one.html")))
(make-dr "second-dr")  






#|
  #;(check-equal? (file->string (->complete-path "one.html")) "(root (id one) first-dr) two")

  #;(report 'step-2a)
  #;(render-to-file-if-needed (->complete-path "two.html.pm"))
  #;(report 'step-2b)
  #;(check-equal? (file->string (->complete-path "two.html")) "(root (id two) first-dr) one")
  (make-dr "second-dr")  
  (reset-cache)
  #;(report 'step-3a)
  #;(render-to-file-if-needed (->complete-path "one.html.pm"))
  #;(report 'step-3b)
  #;(check-equal? (file->string (->complete-path "one.html")) "(root (id one) second-dr) two")
  #;(report 'step-4a)
  #;(render-to-file-if-needed (->complete-path "two.html.pm"))
  #;(report 'step-4b)
  #;(check-equal? (file->string (->complete-path "two.html")) "(root (id two) second-dr) one")
|#
