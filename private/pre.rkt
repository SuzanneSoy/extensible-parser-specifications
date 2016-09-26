#lang racket/base

(require syntax/parse
         (for-syntax racket/base
                     syntax/parse
                     racket/syntax
                     phc-toolkit/untyped)
         "parameters.rkt"
         "no-order.rkt"
         "nop.rkt")

(provide ~pre-check
         ~pre-fail
         ~named-seq
         ~maybe/empty)

(define-eh-mixin-expander ~pre-check
  (λ (stx)
    (syntax-case stx ()
      [(_ pat post)
       (begin (eh-pre-accumulate! '~pre-check #'post)
              #'pat)]
      [(_ post)
       (begin (eh-pre-accumulate! '~pre-check #'post)
              #'(~nop))])))

(define-for-syntax (pre-fail stx)
  (syntax-case stx ()
    [(_ message #:when condition)
     (let ()
       (define/with-syntax clause-present (get-new-clause!))
       (eh-pre-accumulate! '~pre-fail
                           #`(~fail #:when (and (attribute clause-present)
                                                condition)
                                    message))
       #'(~bind [clause-present #t]))]
    [(self #:when condition message)
     (pre-fail #'(self message #:when condition))]
    [(self message #:unless unless-condition)
     (pre-fail #'(self message #:when (not unless-condition)))]
    [(self #:unless unless-condition message)
     (pre-fail #'(self message #:when (not unless-condition)))]))

(define-eh-mixin-expander ~pre-fail pre-fail)

;; TODO: fixme: should happen before the other pre operations
(define-eh-mixin-expander ~named-seq
  (λ (stx)
    (syntax-case stx ()
      [(_ id . pats)
       (identifier? #'id)
       (let ()
         (define/with-syntax clause-present (get-new-clause!))
         (define/with-syntax clause (get-new-clause!))
         (eh-first-accumulate! '~named-seq
                               #'(~bind [(id 1) (if (attribute clause-present)
                                                    (attribute clause)
                                                    (list))]))
         #'(~and (~bind [clause-present #t])
                 (~seq clause (... ...))
                 (~seq . pats)))])))


;; TODO: fixme: should happen before the other pre operations
(define-eh-mixin-expander ~maybe/empty
  (λ (stx)
    (syntax-case stx ()
      [(_ pat …)
       (let ()
         (define/with-syntax clause-present (get-new-clause!))
         (define/with-syntax (expanded-pat …)
           ;; let the ~post, ~global etc. within pat … be recognized
           (expand-all-eh-mixin-expanders #'(pat …)))
         (eh-first-accumulate! '~maybe/empty
                               #'(~parse (expanded-pat …)
                                         (if (attribute clause)
                                             #'(clause (... ...))
                                             #'())))
         #'{~optional {~and {~bind [clause-present #t]}
                            {~seq clause (... ...)}}})])))