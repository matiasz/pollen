#lang racket/base
(require (for-syntax racket/base racket/syntax "../setup.rkt" "split-metas.rkt")
         "to-string.rkt" "../pagetree.rkt" "splice.rkt" "../setup.rkt")
(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [dialect-module-begin #%module-begin]))

;; PARSER-MODE-FROM-READER is used when Pollen is invoked as a specific dialect #lang, and the expander is the generic `pollen/main`.
;; PARSER-MODE-FROM-EXPANDER is used when Pollen is invoked through an inline submodule,
;; and the expander is a specific dialect: `pollen/markup`, `pollen/pre`, etc.
;; they only differ in the `pollen/main` dialect, where the reader infers the dialect from the file extension.

(define here #t)

(define-syntax-rule (dialect-module-begin PARSER-MODE-FROM-EXPANDER READER-SUBMOD)
  (#%module-begin
   (require racket/base)
   (provide (except-out (all-from-out racket/base) #%module-begin)
            (rename-out [pmb #%module-begin]))
   (define-syntax (pmb stx)
     (syntax-case stx ()
       [(_ . EXPRS) (with-syntax ([EXPRS (syntax-property #'EXPRS 'parser-mode-from-expander 'PARSER-MODE-FROM-EXPANDER)])
                      #'(pollen-module-begin . EXPRS))]))
   READER-SUBMOD))


(define (make-parse-proc parser-mode root-proc)
  (define (stringify xs) (apply string-append (map to-string xs)))
  (cond
    [(eq? parser-mode default-mode-pagetree) decode-pagetree]
    [(eq? parser-mode default-mode-markup) (λ (xs) (apply root-proc (remove-voids xs)))] 
    [(eq? parser-mode default-mode-markdown)
     (λ (xs) (let* ([xs (stringify xs)]
                    [xs ((dynamic-require 'markdown 'parse-markdown) xs)]
                    [xs (map strip-empty-attrs xs)])
               (apply root-proc xs)))]
    [else stringify])) ; preprocessor mode


(define (strip-leading-newlines doc)
  ;; drop leading newlines, as they're often the result of `defines` and `requires`
  (or (memf (λ (ln) (not (equal? ln (setup:newline)))) doc) null))

(require (for-syntax syntax/strip-context pollen/core racket/list) racket/splicing)
(define-syntax (pollen-module-begin stx)
  (syntax-case stx ()
    [(_ . EXPRS)
     (with-syntax* (;; need to replace context so these exprs can absorb bindings introduced by this macro
                    [EXPRS (replace-context #'here #'EXPRS)]
                    [PARSER-MODE-FROM-READER (syntax-property stx 'parser-mode-from-reader)]
                    [PARSER-MODE-FROM-EXPANDER (syntax-property #'EXPRS 'parser-mode-from-expander)]
                    [((KV ...) LAST) (syntax-case (local-transformer-expand/capture-lifts #`#,(drop (syntax->list #'EXPRS) 2) 'expression null) ()
                                [(begin (define-values (ID) (K V)) ... LAST) #'(('(K V) ...) LAST)])]
                    [META-HASH #'(apply hasheq (apply append (list KV ...)))]
                    [METAS-ID (setup:meta-export)]
                    [META-MOD-ID (setup:meta-export)]
                    [ROOT-ID (setup:main-root-node)]
                    [DOC-ID (setup:main-export)]
                    ;; prevents conflicts with other imported Pollen sources
                    [DOC-RAW (replace-context #'here (generate-temporary 'pollen-))])
       
       #`(#%module-begin
          (require pollen/top) ; could be at top of this module, but better to contain it
              
          (module META-MOD-ID racket/base
            (provide METAS-ID)
            (define METAS-ID META-HASH))
                
          (module inner pollen/private/doclang-raw
            DOC-RAW ; positional arg for doclang-raw that sets name of export
            (require pollen/top pollen/core pollen/setup (submod ".." META-MOD-ID))
            (provide (all-defined-out) METAS-ID)
            (splicing-syntax-parameterize ([define-meta (make-rename-transformer #'void)])
              . EXPRS))
          (require 'inner)

          (define DOC-ID
            ;; parser-mode must be resolved at runtime, not compile time
            (let* ([parser-mode (or 'PARSER-MODE-FROM-READER PARSER-MODE-FROM-EXPANDER)]
                   [proc (make-parse-proc parser-mode ROOT-ID)] 
                   [doc-elements (strip-leading-newlines DOC-RAW)]
                   [doc-elements-spliced (splice doc-elements (setup:splicing-tag))])
              (proc doc-elements-spliced)))
                
          (provide DOC-ID (except-out (all-from-out 'inner) DOC-RAW))))]))
