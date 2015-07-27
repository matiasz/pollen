#lang racket/base

(module inner racket/base
  (require "two.rkt")
  (provide proc))

(require 'inner)
(provide proc)