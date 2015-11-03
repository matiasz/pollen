#lang pollen/exp racket/base
(require racket/string)

(define (proc)
  (apply string-join (string-split ∆string-append{foo bar zam}) ∆'{X}))

(display (proc))

(module test racket/base
  ;; Racket 6.0 does not support test-omit-paths
  ;; and it chokes on this file, so suppress the testing
  )