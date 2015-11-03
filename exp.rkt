#lang racket/base

(module reader racket/base
  (require syntax/module-reader pollen/world
           (only-in scribble/reader make-at-readtable))
  
  (provide (rename-out [at-read read]
                       [at-read-syntax read-syntax]
                       [at-get-info get-info]))
  
  (define (wrap-reader p)
    (λ args
      (parameterize ([current-readtable (make-at-readtable #:datum-readtable 'dynamic
                                                           #:command-readtable 'dynamic
                                                           #:command-char (world:current-command-char))])
        (apply p args))))
  
  (define-values (at-read at-read-syntax at-get-info)
    (make-meta-reader
     'pollen/exp
     "language path"
     (λ(bstr)
       (let* ([str (bytes->string/latin-1 bstr)]
              [sym (string->symbol str)])
         (and (module-path? sym)
              (vector
               ;; try submod first:
               `(submod ,sym reader)
               ;; fall back to /lang/reader:
               (string->symbol (string-append str "/lang/reader"))))))
     wrap-reader
     (λ(orig-read-syntax)
       (define read-syntax (wrap-reader orig-read-syntax))
       (λ args
         (define stx (apply read-syntax args))
         (define old-prop (syntax-property stx 'module-language))
         (define new-prop `#(pollen/exp/lang/language-info get-language-info ,old-prop))
         (syntax-property stx 'module-language new-prop)))
     (λ(proc)
       (λ(key defval)
         (define (fallback) (if proc (proc key defval) defval))
         (define (try-dynamic-require mod export)
           (or (with-handlers ([exn:fail? (λ(x) #f)])
                 (dynamic-require mod export))
               (fallback)))
         (case key
           [(color-lexer)
            (try-dynamic-require 'syntax-color/scribble-lexer 'scribble-lexer)]
           [(definitions-text-surrogate)
            'scribble/private/indentation]
           [(drracket:indentation)
            (dynamic-require 'scribble/private/indentation 'determine-spaces)]
           [else (fallback)]))))))