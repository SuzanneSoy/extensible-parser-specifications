#lang racket/base

(require syntax/parse
         (for-syntax racket/base))

(define-syntax ~mutex
  (pattern-expander
   (λ (stx)
     (syntax-case stx ()
       [(self (mutex:id ...) pat ...)
        #'(NotImplementedYet...)]))))