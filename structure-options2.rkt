#lang racket

(require racket/require
         syntax/parse
         (subtract-in syntax/stx phc-toolkit/untyped)
         rackunit
         racket/format
         phc-toolkit/untyped
         (for-syntax syntax/parse
                     syntax/stx
                     racket/format))

(require "structure-options2b.rkt")

(provide structure-kw-instance-or-builder-mixin
         structure-kw-predicate-mixin
         structure-kw-fields-mixin
         structure-kw-all-mixin)

(define-eh-alternative-mixin structure-kw-instance-or-builder
  (pattern (~optional (~and instance-or-builder
                            (~or (~and instance #:instance)
                                 (~and builder #:builder)))
                      #:name "either #:instance or #:builder")))

(define-eh-alternative-mixin structure-kw-predicate
  (pattern (~optional (~seq #:? predicate:id)
                      #:name "#:? predicate")))

(define-and-for-syntax instance-no-values-error
  (~a "The #:instance keyword implies the use of [field value],"
      " [field : type value] or [field value : type]."))
(define-eh-alternative-mixin structure-kw-fields
  (pattern (~once (~seq [field:id] ...
                        (~post-fail instance-no-values-error
                                    #:when (and (attribute instance)
                                                (not (stx-null? #'(field ...))))))
                  #:name "[field]")))

(define-eh-alternative-mixin structure-kw-all
  (pattern (~or (structure-kw-instance-or-builder-mixin)
                (structure-kw-predicate-mixin)
                (structure-kw-fields-mixin))))

;; ---------

(define-splicing-syntax-class structure-kws
  (pattern #;(~no-order (structure-kw-all-mixin))
           (~delimit-cut
            (~and
             (~seq
              (~or
               (~optional
                (~and
                 instance-or-builder
                 (~or (~and instance #:instance) (~and builder #:builder)))
                #:name
                "either #:instance or #:builder")
               (~optional (~seq #:? predicate:id) #:name "#:? predicate")
               (~optional (~seq (field:id) ...+ (~bind [clause178673 #t]))
                          #:name "[field]"))
              ...)
             ~!
             (~fail
              #:when
              (and (attribute clause178673)
                   (and (attribute instance)))
              instance-no-values-error)))))

(check-equal? (syntax->datum
               (syntax-parse #'(#:instance #:? p)
                 [(:structure-kws) #'(instance instance-or-builder predicate)]))
              '(#:instance #:instance p))

(check-equal? (syntax->datum
               (syntax-parse #'(#:builder)
                 [(k:structure-kws) #'(k.builder k.instance-or-builder)]))
              '(#:builder #:builder))

(check-equal? (syntax->datum
               (syntax-parse #'()
                 [(:structure-kws) #'()]))
              '())

;; This one is appropriately rejected :)
(check-exn (regexp (regexp-quote instance-no-values-error))
           (λ ()
             (syntax-parse #'(#:instance [f1] [f2])
               [(:structure-kws) #'([field ...] instance)])))

(check-equal? (syntax->datum
               (syntax-parse #'(#:builder #:? p [f1] [f2])
                 [(:structure-kws) #'([field ...] builder)]))
              '([f1 f2] #:builder))

;; This one is appropriately rejected
(check-exn #px"unexpected term"
           (λ ()
             (syntax-parse #'(#:a)
               [(:structure-kws) 'err])))