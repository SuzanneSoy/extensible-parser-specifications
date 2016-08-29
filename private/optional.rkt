#lang racket/base

(require syntax/parse
         phc-toolkit/untyped
         (for-syntax racket/base
                     syntax/parse
                     phc-toolkit/untyped)
         "parameters.rkt"
         "no-order.rkt")

(provide ~optional/else)

(define-eh-mixin-expander ~optional/else
  (syntax-parser
    [(_ pat
        (~optional (~seq #:defaults (default-binding ...))
                   #:defaults ([(default-binding 1) (list)]))
        (~seq #:else-post-fail (~or (~seq message #:when condition)
                                    (~seq #:when condition message)))
        ...
        (~optional (~seq #:name name)))
     #:with clause-whole (get-new-clause!)
     #:with clause-present (get-new-clause!)
     (for ([message (in-syntax #'(message ...))]
           [condition (in-syntax #'(condition ...))])
       (eh-post-accumulate! '~optional/else
                            #`(~fail #:when (and (eq? (attr clause-present) 0)
                                                 #,condition)
                                     #,message)))
     #`(~optional (~and pat
                        ;(~seq clause-whole (... ...))
                        ;; can't use #f, because of the bug
                        ;; https://github.com/racket/racket/issues/1437
                        (~bind [clause-present 1]))
                  #:defaults (default-binding ...
                               ;[(clause-whole 1) #'()]
                               [clause-present 0])
                  #,@(if (attribute name) #'(#:name name) #'()))]))