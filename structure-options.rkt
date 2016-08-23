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
                         (eh-post-accumulate (quote-syntax post)))
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

;; End eh-mixin
;; ------------

(define-eh-alternative-mixin structure-kw-instance-or-builder
  (pattern (~optional (~and instance-or-builder
                            (~or (~and instance #:instance)
                                 (~and builder #:builder)))
                      #:name "either #:instance or #:builder")))

(define-eh-alternative-mixin structure-kw-predicate
  (pattern (~optional (~seq #:? predicate:id)
                      #:name "#:? predicate")))

(define-eh-alternative-mixin structure-kw-fields
  (pattern (~once (~seq [field:id] ...)
                  #:name "[field]"))
  #:post (~fail #:when (and (attribute instance)
                            (not (stx-null? #'(field ...))))))

(define-eh-alternative-mixin structure-kw-all
  (pattern (~or (structure-kw-instance-or-builder-mixin)
                (structure-kw-predicate-mixin)
                #;(structure-kw-fields-mixin))))


(define-splicing-syntax-class-with-eh-mixins structure-kws
  (pattern (~seq (structure-kw-all-mixin) ...)))

#;(define-splicing-syntax-class
    structure-kws
    (pattern
     (~and
      (~seq
       (~or
        (~or
         (~or
          (~optional
           (~and
            instance-or-builder
            (~or (~and instance #:instance) (~and builder #:builder)))
           #:name
           "either #:instance or #:builder"))
         (~or (~optional (~seq #:? predicate:id)
                         #:name "#:? predicate"))
         (~or (~once (~and (~seq (field:id) ...))
                     #:name "[field] …"))))
       ...)
      (~fail #:when (and (attribute instance)
                         (not (stx-null? #'(field ...))))))))

#;(begin
    (syntax-parse #'(#:instance #:? p)
      [(:structure-kws) #'(instance instance-or-builder predicate)])

    (syntax-parse #'(#:builder)
      [(k:structure-kws) #'(k.builder k.instance-or-builder)])

    (syntax-parse #'()
      [(:structure-kws) #'()])

    (syntax-parse #'(#:instance #:? p [f1] [f2])
      [(:structure-kws) #'([field ...] instance)])

    (syntax-parse #'(#:builder [f1] [f2])
      [(:structure-kws) #'([field ...] builder)]))

#;(syntax-parse #'(#:a)
    [(:structure-kws) 'err])