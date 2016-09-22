#lang racket

(require syntax/parse
         phc-toolkit/untyped
         (for-syntax racket/base
                     syntax/parse
                     phc-toolkit/untyped)
         "no-order.rkt"
         "pre.rkt"
         "parameters.rkt")

(provide ~before
         ~after
         ~try-before
         ~try-after)

(define-eh-mixin-expander ~before
  (λ (stx)
    (syntax-case stx ()
      [(_ other message pat …)
       (and (identifier? #'other)
            (string? (syntax-e #'message)))
       (with-syntax ([pt (get-new-clause!)])
         #'{~order-point pt
             {~seq pat …}
             {~pre-fail message #:when (order-point> pt other)}})])))

(define-eh-mixin-expander ~after
  (λ (stx)
    (syntax-case stx ()
      [(_ other message pat …)
       (and (identifier? #'other)
            (string? (syntax-e #'message)))
       (with-syntax ([pt (get-new-clause!)])
         #'{~order-point pt
             {~seq pat …}
             {~pre-fail message #:when (order-point< pt other)}})])))

(define-eh-mixin-expander ~try-before
  (λ (stx)
    (syntax-case stx ()
      [(_ other message pat …)
       (and (identifier? #'other)
            (string? (syntax-e #'message)))
       (with-syntax ([pt (get-new-clause!)])
         #'{~order-point pt
             {~seq pat …}
             {~pre-fail message #:when (try-order-point> pt other)}})])))

(define-eh-mixin-expander ~try-after
  (λ (stx)
    (syntax-case stx ()
      [(_ other message pat …)
       (and (identifier? #'other)
            (string? (syntax-e #'message)))
       (with-syntax ([pt (get-new-clause!)])
         #'{~order-point pt
             {~seq pat …}
             {~pre-fail message #:when (try-order-point< pt other)}})])))