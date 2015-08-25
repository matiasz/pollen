#lang racket/base
(require sugar/define txexpr sugar/coerce pollen/world racket/string racket/list (prefix-in x: xml) (prefix-in h: html) net/url racket/port)

(module+ test
  (require rackunit))

(define (attrs->pollen attrs)
  (string-join (flatten (map (λ(pair) (list (format "'~a:" (car pair)) (format "\"~a\"" (cadr pair)))) attrs)) " "))

(define+provide/contract (xexpr->pollen x)
  (xexpr? . -> . string?)
  (let loop ([x x])
    (cond
      [(txexpr? x) (define-values (tag attrs elements) (txexpr->values x))
                   (string-append* 
                    (map ->string  `(,(world:current-command-char) ,tag 
                                                                   ,@(if (not (empty? attrs))
                                                                         `("[" ,(attrs->pollen attrs) "]")
                                                                         empty) 
                                                                   ,@(if (not (empty? elements))
                                                                         `("{" ,@(map loop elements) "}" )
                                                                         empty))))]
      [(symbol? x) (format "◊string->symbol{~a}" x)]
      [(x:valid-char? x) (format "◊string->number{~a}" x)]
      [else x])))


(define+provide/contract (html->xexpr html-string)
  (string? . -> . xexpr?)
  (parameterize ([x:permissive-xexprs #t]
                 [x:xexpr-drop-empty-attributes #t])
    `(xexpr ,@(map x:xml->xexpr (h:read-html-as-xml (open-input-string html-string))))))

(module+ test
  (check-equal? (html->xexpr "<script>4 > 3</script>") '(xexpr (script "4 > 3"))))

(define+provide/contract (url->xexpr url-or-string)
  ((or/c string? url?) . -> . xexpr?)
  (define url (if (string? url-or-string) (string->url url-or-string) url-or-string))
  (html->xexpr (port->string (get-pure-port url))))


(define+provide/contract (html->pollen html-string)
  (string? . -> . string?)
  (xexpr->pollen (html->xexpr html-string))) 


(define+provide/contract (url->pollen url-or-string #:p-breaks [p-breaks #f])
  (((or/c string? url?)) (#:p-breaks boolean?) . ->* . string?)
  (define url (if (string? url-or-string) (string->url url-or-string) url-or-string))
  (define url-result (port->string (get-pure-port url)))
  (html->pollen url-result  #:p-breaks p-breaks))

(module+ main
  ; (xexpr->pollen '(p "You are puppy"))
  ; (xexpr->pollen '(p ((class "foo")) "You are puppy"))
  ; (xexpr->pollen '(p ((class "foo")) "You are" "\n\n" "puppy"))
  ; (xexpr->pollen '(p ((class "foo")) "You are " (em "so") " puppy"))
  ;  (display (html->pollen #:p-breaks #t (file->string "index.html"))))
  )

(require racket/match)
(define (extract-pcdata some-content)
    (cond [(x:pcdata? some-content)
           (list (x:pcdata-string some-content))]
          [(x:entity? some-content)
           (list)]
          [else
           (extract-pcdata-from-element some-content)]))

(define (extract-pcdata-from-element an-html-element)
    (match an-html-element
      [(struct h:html-full (attributes content))
       (apply append (map extract-pcdata content))]
 
      [(struct h:html-element (attributes))
       '()]))


(define h (h:read-html (open-input-string "<script>4 > 3</script>")))