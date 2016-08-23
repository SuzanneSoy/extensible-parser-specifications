#lang racket

(require "structure-options2b.rkt")


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
                (structure-kw-fields-mixin))))


#;(define-splicing-syntax-class-with-eh-mixins structure-kws
    (pattern (~no-order (structure-kw-all-mixin) ...)))
(define-splicing-syntax-class structure-kws
  #;(pattern (~no-order (structure-kw-all-mixin)))
  (pattern (~and
            (~seq
             (~or
              (~optional
               (~and
                instance-or-builder
                (~or (~and instance #:instance) (~and builder #:builder)))
               #:name
               "either #:instance or #:builder")
              (~optional (~seq #:? predicate:id) #:name "#:? predicate")
              (~once (~seq (field:id) ...) #:name "[field]"))
             ...)
            (~fail #:when (and (attribute instance) (not (stx-null? #'(field ...))))))))

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