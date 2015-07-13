#lang racket/base
(require (only-in scribble/reader make-at-reader) pollen/world racket/path pollen/project syntax/parse racket/syntax)

(provide define+provide-reader-in-mode (all-from-out pollen/world))


(define (make-custom-read custom-read-syntax-proc) 
  (λ(in) (syntax->datum (custom-read-syntax-proc (object-name in) in))))

(require sugar/debug describe)
(define (make-custom-read-syntax reader-mode)
  (λ (src in)
    (define read-inner (make-at-reader 
                        #:command-char (if (or (equal? reader-mode world:mode-template) 
                                               (and (string? src) (regexp-match (pregexp (format "\\.~a$" (world:current-template-source-ext))) src)))
                                           (world:current-template-command-char)
                                           (world:current-command-char))
                        #:syntax? #t 
                        #:inside? #t))
    (define file-contents-stx (read-inner src in))
    (syntax-case file-contents-stx ()
      [(body ...)
       (with-syntax
       ([my-reader-mode (format-symbol "~a" reader-mode)]
       [my-reader-here-path (cond [(symbol? src) (symbol->string src)]
                                        [(equal? src "unsaved editor") src]
                                        [else (path->string src)])]
       [r+p-d-r-f (datum->syntax file-contents-stx (require+provide-directory-require-files src))]
       [main-export (format-id file-contents-stx "~a" (world:current-main-export))])
       #'(module pollen-lang-module pollen
             (define reader-mode 'my-reader-mode)
             (define reader-here-path my-reader-here-path)
             (define parser-mode
               (if (equal? reader-mode world:mode-auto)
                   (let* ([file-ext-pattern (pregexp "\\w+$")]
                          [here-ext (string->symbol (car (regexp-match file-ext-pattern reader-here-path)))])
                     (cond
                       [(equal? here-ext (world:current-pagetree-source-ext)) world:mode-pagetree]
                       [(equal? here-ext (world:current-markup-source-ext)) world:mode-markup]
                       [(equal? here-ext (world:current-markdown-source-ext)) world:mode-markdown]
                       [else world:mode-preproc]))
                   reader-mode))
             ;; change names of exports for local use
             ;; so they don't conflict if this source is imported into another
             (provide (except-out (all-defined-out) reader-here-path reader-mode parser-mode)
                      (prefix-out inner: reader-here-path)
                      (prefix-out inner: reader-mode)
                      (prefix-out inner: parser-mode)) 
             
             r+p-d-r-f
             body ...))])))


(define-syntax-rule (define+provide-reader-in-mode mode)
  (begin
    (define reader-mode mode)
    (define custom-read-syntax (make-custom-read-syntax reader-mode))
    (define custom-read (make-custom-read custom-read-syntax))
    (define (get-info in mod line col pos)
      (λ (key default)
        (case key
          [(color-lexer)
           (define make-scribble-inside-lexer2
             (dynamic-require 'syntax-color/scribble-lexer 'make-scribble-inside-lexer (λ () #f)))
           (cond [make-scribble-inside-lexer2
                  (make-scribble-inside-lexer2 #:command-char #\◊)]
                 [else default])]
          [else default])))
    (provide (rename-out [custom-read read] [custom-read-syntax read-syntax]) get-info)))