#lang sugar/debug racket/base
(require racket/file file/cache sugar/coerce "project.rkt" "world.rkt" racket/rerequire)

;; The cache is a hash with paths as keys.
;; The cache values are also hashes, with key/value pairs for that path.

(provide (all-defined-out))

(define cache-dir
  (build-path (world:current-project-root) (world:current-cache-dir-name)))


(define (reset-cache)
  (cache-remove #f cache-dir))


(define (paths->key source-path [template-path #f])
  ;; key is list of file + mod-time pairs
  (define path-strings (map (compose1 ->string ->complete-path)
                            (append (list source-path)
                                    (if template-path (list template-path) null)
                                    (or (get-directory-require-files source-path) null))))
  (map cons path-strings (map file-or-directory-modify-seconds path-strings)))



(define (update-directory-requires source-path)
  (define directory-require-files (get-directory-require-files source-path))
  (and directory-require-files (map dynamic-rerequire directory-require-files))
  (void))


(define (path->hash path)
  #Rpath
  (dynamic-rerequire #Rpath)
  (displayln (file->string (build-path (world:current-project-root) "directory-require.rkt")))
  #R(dynamic-require (build-path (world:current-project-root) "directory-require.rkt") 'color)
  (hash (world:current-main-export) #R(dynamic-require path (world:current-main-export))
        (world:current-meta-export) #R(dynamic-require path (world:current-meta-export))))

(require sugar/debug)
(define (cached-require path-string subkey [signal #f])  
  (define path (with-handlers ([exn:fail? (λ _ (error 'cached-require (format "~a is not a valid path" path-string)))])
                 (->complete-path path-string)))
  
  (when (not (file-exists? path))
    (error (format "cached-require: ~a does not exist" path)))
  
  (cond
    [(world:current-compile-cache-active)
     (define pickup-file (build-path cache-dir "pickup.rktd"))
     (let ([key (paths->key path)])
       (cache-file pickup-file
                   #:exists-ok? #t
                   key
                   cache-dir
                   #:notify-cache-use (λ(cfn) (displayln (format "cr1: using ~a for key ~a" cfn key))) 
                   (λ _ (displayln (format "cr1: caching key ~a" key))
                     (write-to-file (path->hash path) pickup-file #:exists 'replace))
                   #:max-cache-size (world:current-compile-cache-max-size)))
     (hash-ref (file->value pickup-file) subkey)]
    [signal 
     (report signal '|cache deactivated by signal|)
     (dynamic-require path subkey)]
    [else (dynamic-require path subkey)]))



(define (cached-require2 path-string subkey [signal #f])  
  (define path (->complete-path path-string))
  
  (when signal #Rsignal)
  #|
(define pickup-file (build-path cache-dir "pickup.rktd"))
  #;(let ([key (cons 'template (paths->key path))])
    (cache-file pickup-file
                #:exists-ok? #t
                key
                cache-dir
                #:notify-cache-use (λ(cfn) (displayln (format "cr2: using ~a for key ~a" cfn key))) 
                (λ _ (displayln (format "cr2: caching key ~a" key))
                  (write-to-file (path->hash path) pickup-file #:exists 'replace))))
  #;(hash-ref (file->value pickup-file) subkey)

 |#
  (dynamic-require path subkey))
