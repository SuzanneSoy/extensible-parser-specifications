#lang racket
(require syntax/parse
         (for-syntax syntax/parse
                     phc-toolkit/untyped))

(provide ~nop)

(define-syntax ~nop
  (pattern-expander
   (λ/syntax-case (_) () #'(~do))))
