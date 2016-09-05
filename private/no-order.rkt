#lang racket/base

(require syntax/parse
         ;syntax/parse/experimental/eh
         generic-syntax-expanders
         phc-toolkit/untyped
         (for-syntax racket/base
                     syntax/parse
                     racket/syntax
                     phc-toolkit/untyped
                     racket/list
                     racket/pretty)
         "parameters.rkt")

(provide define-eh-alternative-mixin
         ~seq-no-order
         ~no-order
         (expander-out eh-mixin))

(define-expander-type eh-mixin)

(define-syntax define-eh-alternative-mixin
  (syntax-parser
    [(_ name (~maybe #:define-splicing-syntax-class splicing-name)
        ((~literal pattern) pat) ...)
     #`(begin
         (define-eh-mixin-expander name
           (λ (_)
             (quote-syntax (~or pat ...))))
         #,@(if (attribute splicing-name)
                #'((define-splicing-syntax-class splicing-name
                     (pattern {~seq-no-order {name}})))
                #'()))]))

(define-for-syntax (inline-or stx)
  (syntax-case stx ()
    [(o . rest)
     (and (identifier? #'o) (free-identifier=? #'o #'~or))
     (apply append (stx-map inline-or #'rest))]
    [x (list #'x)]))

;; TODO: ~seq-no-order should also be a eh-mixin-expander, so that when there
;; are nested ~seq-no-order, the ~post-fail is caught by the nearest
;; ~seq-no-order.
(define-syntax ~seq-no-order
  (pattern-expander
   (λ (stx)
     (syntax-case stx ()
       [(self pat ...)
        ((λ (x) #;(pretty-write (syntax->datum x)) x)
         (with-disappeared-uses
           (define counter 0)
           (define (increment-counter)
             (begin0 counter
                     (set! counter (add1 counter))))
           ;; post-acc gathers some a-patterns which will be added after the
           ;; (~seq (~or ) ...)
           (define post-acc '())
           (define (add-to-post! v) (set! post-acc (cons v post-acc)))
           ;; post-groups-acc gathers some attributes that have to be grouped
           (define post-groups-acc '())
           (define (add-to-post-groups! . v)
             (set! post-groups-acc (cons v post-groups-acc)))
           ;; expand EH alternatives:
           (define alts
             (parameterize ([eh-post-accumulate add-to-post!]
                            [eh-post-group add-to-post-groups!]
                            [clause-counter increment-counter])
               ;(inline-or
               (expand-all-eh-mixin-expanders #'(~or pat ...))))
           (define post-group-bindings
             (for/list ([group (group-by car
                                         post-groups-acc
                                         free-identifier=?)])
               ;; each item in `group` is a four-element list:
               ;; (list result-id aggregate-function attribute)
               (define/with-syntax name (first (car group))
                 #;(syntax-local-introduce
                    (datum->syntax #'here
                                   (first (car group)))))
               (define/with-syntax f (second (car group)))
               #`[name (f . #,(map (λ (i) #`(attribute #,(third i)))
                                   group))]))
           #`(~delimit-cut
              (~and (~seq #,alts (... ...)) ;;(~or . #,alts)
                    ~!
                    (~bind #,@post-group-bindings)
                    #,@post-acc))))]))))

(define-syntax ~no-order
  (pattern-expander
   (λ/syntax-case (_ . rest) ()
     #'({~seq-no-order . rest}))))