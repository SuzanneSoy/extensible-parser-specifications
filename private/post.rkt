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
         ~post-fail
         ~named-seq)

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

#;(define-eh-mixin-expander ~defaults
    (λ (stx)
      (syntax-case stx ()
        [(_ ([a v] ...) . pats)
         (let ()
           (define/with-syntax clause-present (get-new-clause!))
           (eh-post-accumulate! '~defaults
                                #'(~bind [a (or (attribute clause-present) v)]
                                         ...))
           #'(~and (~bind [clause-present #t]) . pats))])))

(define-eh-mixin-expander ~named-seq
  (λ (stx)
    (syntax-case stx ()
      [(_ id . pats)
       (identifier? #'id)
       (let ()
         (define/with-syntax clause-present (get-new-clause!))
         (define/with-syntax clause (get-new-clause!))
         (eh-post-accumulate! '~named-seq
                              #'(~bind [(id 1) (if (attribute clause-present)
                                                   (attribute clause)
                                                   (list))]))
         #'(~and (~bind [clause-present #t])
                 (~seq clause (... ...))
                 (~seq . pats)))])))

(define-for-syntax (post-fail stx)
  (syntax-case stx ()
    [(_ message #:when condition)
     (let ()
       (define/with-syntax clause-present (get-new-clause!))
       (eh-post-accumulate! '~post-fail
                            #`(~fail #:when (and (attribute clause-present)
                                                 condition)
                                     message))
       #'(~bind [clause-present #t]))]
    [(self #:when condition message)
     (post-fail #'(self message #:when condition))]
    [(self message #:unless unless-condition)
     (post-fail #'(self message #:when (not unless-condition)))]
    [(self #:unless unless-condition message)
     (post-fail #'(self message #:when (not unless-condition)))]))

(define-eh-mixin-expander ~post-fail post-fail)