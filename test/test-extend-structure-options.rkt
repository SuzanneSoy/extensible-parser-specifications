#lang racket

(require rackunit
         phc-toolkit/untyped
         (for-syntax extensible-parser-specifications
                     "test-structure-options.rkt"
                     syntax/parse))

(define-syntax/parse+simple [foo foo-a :structure-kws]
  #''(foo-a field ...))

(check-equal? (foo #:first-case  [f tf] [g tg])
              '(#:first-case f g))

(begin-for-syntax
  (define-splicing-syntax-class structure-xyz-kws
    (pattern {~seq-no-order {~optional {~seq #:xyz xyz:id}}
                            {structure-kw-all-mixin}})))

(define-syntax/parse [bar foo-a :structure-xyz-kws]
  #`'[(xyz foo-a field ...)
      #,(foo-forward-attributes)])

(check-equal? (bar #:second-case #:xyz zyx [f tf] [g tg])
              '((zyx #:second-case f g)
                (quote (#:second-case f g))))
