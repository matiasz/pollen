#lang racket/base

(module inner racket/base
  (require "directory-require.rkt")
  (provide do))

(require 'inner)
(provide do)