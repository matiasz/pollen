#lang racket
;; case3: cross-referenced pages
(require sugar rackunit racket/file pollen/rerequire)


(define (make-dr dr-path arg)
  (sleep 1)
  (display-to-file (format "#lang racket/base
(provide do)
(define (do) ~v)" arg) dr-path #:exists 'replace)
  ;; display-to-file doesn't change mod=time, so change it manually
  (file-or-directory-modify-seconds dr-path (current-seconds)))


(make-dr "directory-require.rkt" "foo")
;(file-or-directory-modify-seconds "two.rkt")
(check-true (and (member (->complete-path "directory-require.rkt") (dynamic-rerequire "one.rkt")) #t))
(check-true (and (member (->complete-path "directory-require.rkt") (dynamic-rerequire "one.html.pm")) #t))
(check-equal? ((dynamic-require "one.rkt" 'do)) "foo")
(check-equal? (cadr (dynamic-require "one.html.pm" 'doc)) "foo")
(make-dr "directory-require.rkt" "zam") 
;(file-or-directory-modify-seconds "two.rkt")
(check-true (and (member (->complete-path "directory-require.rkt") (dynamic-rerequire "one.rkt")) #t))
(check-true (and (member (->complete-path "directory-require.rkt") (dynamic-rerequire "one.html.pm")) #t))
(check-equal? ((dynamic-require "one.rkt" 'do)) "zam")
(check-equal? (cadr (dynamic-require "one.html.pm" 'doc)) "zam")


