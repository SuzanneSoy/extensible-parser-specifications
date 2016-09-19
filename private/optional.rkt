#lang racket/base

(require syntax/parse
         phc-toolkit/untyped
         (for-syntax racket/base
                     syntax/parse
                     phc-toolkit/untyped)
         "parameters.rkt"
         "no-order.rkt")

(provide ~optional/else)

(begin-for-syntax
  (define-splicing-syntax-class else-post-fail
    (pattern (~seq #:else-post-fail message #:when condition))
    (pattern (~seq #:else-post-fail #:when condition message))
    (pattern (~seq #:else-post-fail message #:unless unless-condition)
             #:with condition #'(not unless-condition))
    (pattern (~seq #:else-post-fail #:when unless-condition message)
             #:with condition #'(not unless-condition))))
   

(define-eh-mixin-expander ~optional/else
  (syntax-parser
    [(_ pat
        (~optional (~seq #:defaults (default-binding ...))
                   #:defaults ([(default-binding 1) (list)]))
        :else-post-fail
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