#lang racket/base
(provide (all-defined-out))

(define (root . xs)
  `(rootover ,@xs))

(module config racket/base
  (provide (all-defined-out))
  
  (define command-char #\∆))