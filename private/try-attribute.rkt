#lang racket/base

(require (for-syntax racket/base))

(provide try-attribute if-attribute)

(define-syntax (if-attribute stx)
  (syntax-case stx ()
    [(_ name if-branch else-branch)
     (if (syntax-pattern-variable? (syntax-local-value #'name (λ () #f)))
         #'if-branch
         #'else-branch)]))

(define-syntax (try-attribute stx)
  (syntax-case stx ()
    [(_ name fallback)
     #'(if-attribute (attribute name) fallback)]
    [(_ name)
     #'(if-attribute (attribute name) #f)]))