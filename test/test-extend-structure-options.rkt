#lang racket

(require rackunit
         phc-toolkit/untyped
         (for-syntax extensible-parser-specifications
                     "test-structure-options.rkt"
                     syntax/parse))

(define-syntax/parse+simple [foo structure-kws]
  #''(field ...))

(check-equal? (foo [f tf] [g tg])
              '(f g))

(begin-for-syntax
  (define-splicing-syntax-class structure-xyz-kws
    (pattern {~seq-no-order {~optional {~seq #:xyz xyz:id}}
                            {structure-kw-all-mixin}})))

(define-syntax/parse [bar :structure-xyz-kws]
  #`'[(xyz field ...)
      #,(foo-forward-attributes)])

(check-equal? (bar #:xyz zyx [f tf] [g tg])
              '((zyx f g) (quote (f g))))
