#lang pollen/exp racket/base
(require racket/string)

(define (proc)
    (apply string-join (string-split ◊string-append{foo bar zam}) ◊'{X}))

(proc)