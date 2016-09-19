#lang racket/base

(require (for-syntax racket/base))

(provide try-attribute)

(define-syntax (try-attribute stx)
  (syntax-case stx ()
    [(_ name)
     (if (syntax-pattern-variable? (syntax-local-value #'name (λ () #f)))
         #'(attribute name)
         #'#f)]))