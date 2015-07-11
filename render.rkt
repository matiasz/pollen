#lang racket/base
(require racket/file racket/path racket/match racket/list racket/string)
(require sugar/test sugar/define sugar/container sugar/file)
(require "file.rkt" "cache.rkt" "world.rkt" "debug.rkt" "pagetree.rkt" "project.rkt" "template.rkt")


;; when you want to generate everything fresh, 
;; but without having to #:force everything.
;; render functions will always go when no mod-time is found.
(define (reset-modification-times)
  (set! modification-time-hash (make-hash)))

;; mod-times is a hash that takes lists of paths as keys,
;; and lists of modification times as values.
(define modification-time-hash #f)
(reset-modification-times)
(module-test-internal
 (check-pred hash? modification-time-hash))

;; using internal contracts to provide some extra safety (negligible performance hit)

(define/contract (valid-path-arg? x)
  (any/c . -> . boolean?)
  (or (equal? #f x) (complete-path? x)))

(define/contract (valid-path-args? x)
  (any/c . -> . boolean?)
  (and (list? x) (andmap valid-path-arg? x)))


(define/contract (make-mod-times-key paths)
  (valid-path-args? . -> . valid-path-args?)
  paths) ; for now, this does nothing; maybe later, it will do more

(module-test-internal
 (require racket/runtime-path)
 (define-runtime-path sample-dir "test/data/samples")
 (define samples (parameterize ([current-directory sample-dir])
                   (map path->complete-path (directory-list "."))))
 (define-values (sample-01 sample-02 sample-03) (apply values samples))
 (check-equal? (make-mod-times-key samples) samples))


(define/contract (path->mod-time-value path)
  ((or/c #f complete-path?) . -> . (or/c #f integer?))
  (and path (file-exists? path) (file-or-directory-modify-seconds path)))

(module-test-internal
 (check-false (path->mod-time-value (path->complete-path "garbage-path.zzz")))
 (check-equal? (path->mod-time-value sample-01) (file-or-directory-modify-seconds sample-01)))


(define/contract (record-modification-time . rest-paths)
  (() #:rest valid-path-args? . ->* . void?)
  (define key (make-mod-times-key rest-paths))
  (hash-set! modification-time-hash key (map path->mod-time-value key)))

(module-test-internal
 (check-equal? (record-modification-time sample-01 sample-02 sample-03) (void))
 (check-true (hash-has-key? modification-time-hash (list sample-01 sample-02 sample-03))))

(define/contract (previously-rendered-together? . rest-paths)
  (() #:rest valid-path-args? . ->* . boolean?)
  (define key (make-mod-times-key rest-paths))
  (hash-has-key? modification-time-hash key))

(require sugar/debug)
(define/contract (modification-time-changed? . rest-paths)
  (() #:rest valid-path-args? . ->* . coerce/boolean?)
  (define key (make-mod-times-key rest-paths))
  (define result (and (apply previously-rendered-together? rest-paths)
                      (ormap (λ(kv hv rp) (and (not (equal? kv hv)) (list rp kv hv))) (map path->mod-time-value key) (hash-ref modification-time-hash key) rest-paths)))
  (and result (message (format "modification-time for ~a changed: was ~a now ~a" (first result) (second result) (third result))) result))

(module-test-internal
 (check-false (previously-rendered-together? sample-01)) ; because key hasn't been stored
 (check-false (apply modification-time-changed? samples))) ; because files weren't changed


(define (list-of-pathish? x) (and (list? x) (andmap pathish? x)))

(define/contract+provide (render-batch . xs)
  (() #:rest list-of-pathish? . ->* . void?)
  ;; Why not just (map render ...)?
  ;; Because certain files will pass through multiple times (e.g., templates)
  ;; And with render, they would be rendered repeatedly.
  ;; Using reset-modification-times is sort of like session control.
  (reset-modification-times) 
  (for-each (λ(x) ((if (pagetree-source? x)
                       render-pagetree
                       render-from-source-or-output-path) x)) xs))


(define/contract+provide (render-pagetree pagetree-or-path)
  ((or/c pagetree? pathish?) . -> . void?)
  (define pagetree (if (pagetree? pagetree-or-path)
                       pagetree-or-path
                       (cached-require pagetree-or-path (world:current-main-export))))
  (parameterize ([current-directory (world:current-project-root)])
    (for-each render-from-source-or-output-path (map ->complete-path (pagetree->list pagetree)))))


(define/contract+provide (render-from-source-or-output-path so-pathish #:force [force #f])
  ((pathish?) (#:force boolean?) . ->* . void?)
  (let ([so-path (->complete-path so-pathish)])  ; so-path = source or output path (could be either) 
    (cond
      [(ormap (λ(test) (test so-path)) (list has/is-null-source? has/is-preproc-source? has/is-markup-source? has/is-scribble-source? has/is-markdown-source? has/is-template-source?)) 
       (let-values ([(source-path output-path) (->source+output-paths so-path)])
         (render-to-file-if-needed source-path output-path #:force force))]
      [(pagetree-source? so-path) (render-pagetree so-path)]))
  (void))


(define/contract (->source+output-paths source-or-output-path)
  (complete-path? . -> . (values complete-path? complete-path?))
  ;; file-proc returns two values, but ormap only wants one
  (define file-proc (ormap (λ(test file-proc) (and (test source-or-output-path) file-proc))
                           (list has/is-null-source? has/is-preproc-source? has/is-markup-source? has/is-scribble-source? has/is-markdown-source? has/is-template-source?)
                           (list ->null-source+output-paths ->preproc-source+output-paths ->markup-source+output-paths ->scribble-source+output-paths ->markdown-source+output-paths ->template-source+output-paths)))
  (file-proc source-or-output-path))


(define/contract (eligible-for-rerequire? source-path)
  (complete-path? . -> . boolean?)
  (not (null-source? source-path))) ; null sources may not have Racket code underneath

(define/contract (rerequire source-path)
  (complete-path? . -> . (listof path?))
  (dynamic-rerequire (->complete-path source-path) #:verbosity 'none))


(define (make-file-path-key source-path [template-path #f])
  (append (list source-path)
          (if template-path
              (list template-path)
              empty)
          (or (and (world:check-directory-requires-in-render?) (get-directory-require-files source-path))
              empty)))


(define/contract+provide (render-needed? source-path [template-path #f] [maybe-output-path #f])
  ((complete-path?) ((or/c #f complete-path?) (or/c #f complete-path?)) . ->* . (or/c #f symbol?))
  (define output-path (or maybe-output-path (->output-path source-path)))
  (when (world:check-directory-requires-in-render?)
    (define directory-require-files (get-directory-require-files source-path))
    (and directory-require-files
         (let ([dr-key (make-mod-times-key directory-require-files)])
           (cond
             [(not (hash-has-key? modification-time-hash dr-key))
              (apply record-modification-time dr-key)]
             [(apply modification-time-changed? dr-key)
              (message "render: directory require files have changed. Resetting cache & file-modification table")
              (reset-cache) ; because stored data is obsolete
              (reset-modification-times) ; this will mark all previously-rendered source files for refresh
              (apply record-modification-time dr-key)])))) ; put mod-time for dr back into table for later
  
  
  (define file-paths (make-file-path-key source-path template-path))
  (or (and (not (file-exists? output-path)) 'output-file-missing)
      (and (not (apply previously-rendered-together? file-paths)) 'not-refreshed-this-session)
      (and (apply modification-time-changed? file-paths) 'modification-time-changed)
      (and (eligible-for-rerequire? source-path) (changed-on-rerequire? source-path) 'source-needed-rerequire)))


(define/contract+provide (render-to-file-if-needed source-path [template-path #f] [maybe-output-path #f] #:force [force #f])
  ((complete-path?) ((or/c #f complete-path?) (or/c #f complete-path?) #:force boolean?) . ->* . void?)  
  (define output-path (or maybe-output-path (->output-path source-path)))
  (define template-path (get-template-for source-path))
  (define reason-to-render (or (and force 'render-forced)
                               (render-needed? source-path template-path output-path)))
  (when reason-to-render
    (message (format "rendering ~a because ~a" (find-relative-path (world:current-project-root) source-path) (string-replace (symbol->string reason-to-render) "-" " ")))
    (render-to-file source-path template-path output-path)))


(define/contract+provide (render-to-file source-path [template-path #f] [maybe-output-path #f])
  ((complete-path?) ((or/c #f complete-path?) (or/c #f complete-path?)) . ->* . void?)
  (define output-path (or maybe-output-path (->output-path source-path)))
  (define render-result (render source-path template-path)) ; will either be string or bytes
  (display-to-file render-result output-path #:exists 'replace
                   #:mode (if (string? render-result) 'text 'binary)))


(define/contract+provide (render source-path [template-path #f])
  ((complete-path?) ((or/c #f complete-path?)) . ->* . (or/c string? bytes?))
  (define render-proc 
    (cond
      [(ormap (λ(pred render-proc) (and (pred source-path) render-proc))
              (list has/is-null-source? has/is-preproc-source? has/is-markup-source? has/is-scribble-source? has/is-markdown-source? has/is-template-source?)
              (list render-null-source render-preproc-source render-markup-or-markdown-source render-scribble-source render-markup-or-markdown-source render-preproc-source))] 
      [else (error (format "render: no rendering function found for ~a" source-path))]))
  
  (message (format "render: ~a" (file-name-from-path source-path)))
  (when (eligible-for-rerequire? source-path) (rerequire source-path))
  (apply record-modification-time (make-file-path-key source-path template-path)) 
  (apply render-proc (list* source-path (if template-path (list template-path) empty))))


(define/contract (render-null-source source-path)
  (complete-path? . -> . bytes?)
  ;; All this does is copy the source. Hence, "null".
  (record-modification-time source-path)
  (file->bytes source-path))


(define/contract (render-scribble-source source-path)
  (complete-path? . -> . string?)
  (match-define-values (source-dir _ _) (split-path source-path))
  (time (parameterize ([current-directory (->complete-path source-dir)])
          ;; BTW this next action has side effects: scribble will copy in its core files if they don't exist.
          ((dynamic-require 'scribble/render 'render) (list (dynamic-require source-path (world:current-main-export))) (list source-path))))
  (define result (file->string (->output-path source-path)))
  (delete-file (->output-path source-path)) ; because render promises the data, not the side effect
  result)


(define/contract (render-preproc-source source-path)
  (complete-path? . -> . (or/c string? bytes?))
  (match-define-values (source-dir _ _) (split-path source-path))
  (record-modification-time source-path)
  (time (parameterize ([current-directory (->complete-path source-dir)])
          (render-through-eval `(begin (require pollen/cache)(cached-require ,source-path ',(world:current-main-export)))))))


(define/contract (render-markup-or-markdown-source source-path [maybe-template-path #f]) 
  ((complete-path?) ((or/c #f complete-path?)) . ->* . (or/c string? bytes?))
  (match-define-values (source-dir _ _) (split-path source-path))
  (define template-path (or maybe-template-path (get-template-for source-path)))
  (render-from-source-or-output-path template-path) ; because template might have its own preprocessor source
  (define expr-to-eval 
    `(begin 
       (require (for-syntax racket/base))
       (require pollen/include-template pollen/cache pollen/debug)
       ,(require-directory-require-files source-path) 
       (let ([,(world:current-main-export) (cached-require ,(path->string source-path) ',(world:current-main-export))]
             [,(world:current-meta-export) (cached-require ,(path->string source-path) ',(world:current-meta-export))])
         (local-require pollen/pagetree pollen/template pollen/top)
         (define here (metas->here ,(world:current-meta-export)))
         (cond 
           [(bytes? ,(world:current-main-export)) ,(world:current-main-export)] ; if main export is binary, just pass it through
           [else
            (include-template #:command-char ,(world:current-command-char) (file ,(->string (find-relative-path source-dir template-path))))]))))
  (time (parameterize ([current-directory source-dir]) ; because include-template wants to work relative to source location
          (render-through-eval expr-to-eval))))


(define/contract (templated-source? path)
  (complete-path? . -> . boolean?)
  (or (markup-source? path) (markdown-source? path)))


(define/contract+provide (get-template-for source-path)
  (complete-path? . -> . (or/c #f complete-path?))
  (match-define-values (source-dir _ _) (split-path source-path))
  (and (templated-source? source-path) ; doesn't make sense if it's not a templated source format
       (let ([output-path (->output-path source-path)])
         (or ; Build the possible paths and use the first one that either exists, or has existing source (template, preproc, or null)
          (ormap (λ(p) (if (ormap file-exists? (list p (->template-source-path p) (->preproc-source-path p) (->null-source-path p))) p #f)) 
                 (filter (λ(x) (->boolean x)) ; if any of the possibilities below are invalid, they return #f 
                         (list                     
                          (parameterize ([current-directory (world:current-project-root)])
                            (message "boingo")
                            (let ([source-metas (cached-require source-path (world:current-meta-export))])
                              (and ((->symbol (world:current-template-meta-key)) . in? . source-metas)
                                   (build-path source-dir (select-from-metas (->string (world:current-template-meta-key)) source-metas))))) ; path based on metas
                          (and (filename-extension output-path) (build-path (world:current-project-root) 
                                                                            (add-ext (world:current-default-template-prefix) (get-ext output-path))))))) ; path to default template
          (and (filename-extension output-path) (build-path (world:current-server-extras-path) (add-ext (world:current-fallback-template-prefix) (get-ext output-path)))))))) ; fallback template


(define/contract+provide (changed-on-rerequire? source-path)
  (complete-path? . -> . boolean?) ; coercion because dynamic-rerequire needs real complete-path
  ;; use dynamic-rerequire now to force render for cached-require later,
  ;; otherwise the source file will get cached by compiler
  (not (empty? (rerequire source-path))))


;; set up namespace for module caching
(module caching-module racket/base
  (define-namespace-anchor caching-module-nsa)
  (provide caching-module-nsa))
(require 'caching-module)

;; (car (current-eval-namespace-cache)) = namespace containing cached modules
;; (cdr (current-eval-namespace-cache)) = list of cached modules
(define current-eval-namespace-cache (make-parameter (cons (namespace-anchor->namespace caching-module-nsa) '())))

(define/contract+provide (add-module-to-current-eval-cache module-name)
  (symbol? . -> . void?)
  (define cache-ns (car (current-eval-namespace-cache)))
  (define cached-modules (cdr (current-eval-namespace-cache)))
  (when (not (member module-name cached-modules))
    (eval `(require ,module-name) cache-ns)
    (current-eval-namespace-cache (cons cache-ns (cons module-name cached-modules)))))

(define initial-modules-to-cache '(xml
                                   racket/bool
                                   racket/class
                                   racket/contract 
                                   racket/draw
                                   racket/file
                                   racket/format
                                   racket/function
                                   racket/port 
                                   racket/list
                                   racket/match
                                   racket/string
                                   racket/syntax
                                   pollen/cache
                                   pollen/debug
                                   pollen/decode
                                   pollen/file
                                   pollen/include-template
                                   pollen/main
                                   pollen/reader-base
                                   pollen/pagetree
                                   pollen/rerequire 
                                   pollen/tag
                                   pollen/template
                                   pollen/world
                                   pollen/project
                                   sugar
                                   txexpr))


(for-each add-module-to-current-eval-cache initial-modules-to-cache)

(require sugar/debug)
(define/contract (render-through-eval expr-to-eval)
  (list? . -> . (or/c string? bytes?))
  (define cache-ns (car (current-eval-namespace-cache)))
  (define cached-modules (cdr (current-eval-namespace-cache)))
  (parameterize ([current-namespace (make-base-namespace)]
                 #;[current-output-port (current-error-port)]
                 [current-pagetree (make-project-pagetree (world:current-project-root))])
    (for-each (λ(mod-name) (namespace-attach-module cache-ns mod-name)) cached-modules)   
    (eval expr-to-eval (current-namespace))))
