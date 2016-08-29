#lang racket/base

(require syntax/parse
         (for-syntax racket/base
                     syntax/parse
                     racket/syntax
                     phc-toolkit/untyped)
         "parameters.rkt"
         "no-order.rkt")

(provide ~nop
         ~post-check
         ~post-fail)

(define-syntax ~nop
  (pattern-expander
   (λ/syntax-case (_) () #'(~do))))

(define-eh-mixin-expander ~post-check
  (λ (stx)
    (syntax-case stx ()
      [(_ pat post)
       (begin (eh-post-accumulate! '~post-check #'post)
              #'pat)]
      [(_ post)
       (begin (eh-post-accumulate! '~post-check #'post)
              #'(~nop))])))

(define-for-syntax (post-fail stx)
  (syntax-case stx ()
    [(_ message #:when condition)
     (begin
       (define/with-syntax clause-present (get-new-clause!))
       (eh-post-accumulate! '~post-fail
                            #`(~fail #:when (and (attribute clause-present)
                                                 condition)
                                     message))
       #'(~bind [clause-present #t]))]
    [(self #:when condition message)
     (post-fail #'(self message #:when condition))]))

(define-eh-mixin-expander ~post-fail post-fail)