#lang racket

(require syntax/parse
         syntax/parse/experimental/eh
         generic-syntax-expanders
         syntax/stx
         (for-syntax syntax/parse
                     racket/syntax
                     syntax/stx
                     racket/pretty)) ;; debug

;; ------------
;; eh-mixin — TODO: move to phc-toolkit once the PR #7 for reqprov in
;; generic-syntax-expander is merged. Look for "End eh-mixin" below for the end.

(define-expander-type eh-mixin)

(begin-for-syntax
  (define eh-post-accumulate (make-parameter #f)))

(define-syntax define-eh-alternative-mixin
  (syntax-parser
    [(_ name ((~literal pattern) pat) ... (~optional (~seq #:post post)))
     (let ()
       (define/with-syntax mixin (format-id #'name "~a-mixin" #'name))
       ;(display "post:") (displayln (attribute post))
       #`(begin
           (define-eh-mixin-expander mixin
             (λ (_)
               #,@(if (attribute post)
                      #'((unless (eh-post-accumulate)
                           (raise-syntax-error
                            'define-eh-alternative-mixin
                            "#:post used outside of ~no-order"))
                         ((eh-post-accumulate) (quote-syntax post)))
                      #'())
               (quote-syntax (~or pat ...))))
           #;(define-eh-alternative-set name
               #,@(stx-map (λ (p)
                             #`(pattern #,(expand-all-eh-mixin-expanders p)))
                           #'(pat ...)))))]))

(define-for-syntax (define-?-syntax-class-with-eh-mixins original-form)
  (syntax-parser
    [(_ signature {~and opts {~not ({~literal pattern} . _)}} ...
        ({~literal pattern} pat . pat-opts) ...)
     ;((λ (x) (pretty-write (syntax->datum x)) x)
     #`(#,original-form
        signature opts ...
        #,@(stx-map (λ (p po)
                      #`(pattern #,(expand-all-eh-mixin-expanders p) . #,po))
                    #'(pat ...)
                    #'(pat-opts ...)))]))

(define-syntax define-splicing-syntax-class-with-eh-mixins
  (define-?-syntax-class-with-eh-mixins #'define-splicing-syntax-class))

(define-syntax define-syntax-class-with-eh-mixins
  (define-?-syntax-class-with-eh-mixins #'define-syntax-class))



(provide define-splicing-syntax-class-with-eh-mixins
         define-syntax-class-with-eh-mixins
         define-eh-alternative-mixin
         (expander-out eh-mixin))

(define-for-syntax (inline-or stx)
  (syntax-case stx ()
    [(o . rest)
     (and (identifier? #'o) (free-identifier=? #'o #'~or))
     (apply append (stx-map inline-or #'rest))]
    [x (list #'x)]))

(define-syntax ~no-order
  (pattern-expander
   (λ (stx)
     (syntax-case stx ()
       [(_ pat ...)
        ((λ (x) (pretty-write (syntax->datum x)) x)
         (let ()
           (define acc '())
           (define (add-to-acc p)
             (displayln p)
             (newline)
             (set! acc (cons p acc)))
           (define alts
             (parameterize ([eh-post-accumulate add-to-acc])
               (expand-all-eh-mixin-expanders
                #'(~or pat ...))))
           #`(~and (~seq (~or . #,(inline-or alts)) (... ...))
                   #,@acc)))]))))

;; End eh-mixin
;; ------------