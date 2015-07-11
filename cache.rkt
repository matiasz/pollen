#lang racket/base
(require racket/serialize racket/file "world.rkt" "rerequire.rkt" "debug.rkt")

;; The cache is a hash with paths as keys.
;; The cache values are also hashes, with key/value pairs for that path.

(provide reset-cache current-cache make-cache cached-require cache-ref)
(provide (all-from-out "rerequire.rkt"))

(define (get-cache-file-path)
  (build-path (world:current-project-root) (world:current-cache-filename)))

(define (make-cache) 
  (define cache-path (get-cache-file-path))
  (if (file-exists? cache-path)
      (let ()
        (define hash-from-file (deserialize (file->value cache-path)))
        (message (format "got hash from file: ~a" hash-from-file))
        hash-from-file)
      (make-hash)))

(define current-cache (make-parameter #f))

(define (reset-cache)
  (define cache-path (get-cache-file-path))
  (when (file-exists? cache-path)
    (delete-file cache-path))
  (current-cache (make-cache)))

(define (->complete-path path-string)
  (path->complete-path (if (string? path-string) (string->path path-string) path-string)))

(define (cache-ref path-string)
  (hash-ref (current-cache) (->complete-path path-string)))

(define (cache-has-key? path)
  (hash-has-key? (current-cache) path))

(define (add-path-to-cache path)
  (dynamic-rerequire path)
  (hash-set! (current-cache) path (make-hash))
  (define cache-hash (cache-ref path))
  (hash-set! cache-hash 'mod-time (file-or-directory-modify-seconds path))
  (hash-set! cache-hash (world:current-main-export) (dynamic-require path (world:current-main-export)))
  (hash-set! cache-hash (world:current-meta-export) (dynamic-require path (world:current-meta-export)))
  (write-to-file (serialize (current-cache)) (get-cache-file-path) #:exists 'replace)
  (void))

(define (cached-require path-string key)
  #;(message (format "cache kvs: ~a" (current-cache)))
  (define path (with-handlers ([exn:fail? (λ(exn) (error 'cached-require (format "~a is not a valid path" path-string)))])
                 (->complete-path path-string)))
  (cond
    [(and (world:current-require-cache-active) (current-cache))
     (when (not (file-exists? path)) (error (format "cached-require: ~a does not exist" (path->string path))))
     (define reason-to-add-path (or (and (not (cache-has-key? path)) 'path-not-in-cache)
                                    (and (> (file-or-directory-modify-seconds path) (hash-ref (cache-ref path) 'mod-time)) 'mod-time-changed)))
     (cond
       [reason-to-add-path
       (message (format "adding ~a to cache because ~a" path reason-to-add-path)) 
       (add-path-to-cache path)]
       [else (message (format "using cached version of ~a because it's in cache: ~a" path (current-cache)))])
     (hash-ref (cache-ref path) key)]
    [else ; cache inactive
     (dynamic-rerequire path)
     (dynamic-require path key)]))