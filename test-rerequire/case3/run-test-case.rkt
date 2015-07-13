#lang racket
;; case3: cross-referenced pages
(require sugar rackunit racket/file pollen/rerequire)


(define (make-dr dr-path arg)
  (sleep 1)
  (file-or-directory-modify-seconds dr-path (current-seconds))
  (display-to-file (format "#lang racket/base
(provide do)
(define (do) ~v)" arg) dr-path #:exists 'replace))


(make-dr "two.rkt" "foo")
(file-or-directory-modify-seconds "two.rkt")
(void (dynamic-rerequire "one.html.pm"))
(dynamic-require "one.html.pm" 'doc)
(void (dynamic-rerequire "one.rkt"))
((dynamic-require "one.rkt" 'do))
(make-dr "two.rkt" "zam") 
(file-or-directory-modify-seconds "two.rkt")
(dynamic-rerequire "one.rkt")
((dynamic-require "one.rkt" 'do))
(dynamic-rerequire "one.html.pm")
(dynamic-require "one.html.pm" 'doc)



