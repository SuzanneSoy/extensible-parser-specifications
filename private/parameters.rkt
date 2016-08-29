#lang racket/base

(require (for-syntax racket/base))

(provide (for-syntax eh-post-accumulate
                     eh-post-accumulate!
                     eh-post-group
                     eh-post-group!
                     clause-counter
                     get-new-clause!))

(define-syntax-rule (define-dynamic-accumulator-parameter parameter-name name!)
  (begin
    (define-for-syntax parameter-name (make-parameter #f))
    (define-for-syntax (name! name . args)
      (unless (parameter-name)
        (raise-syntax-error name
                            (string-append (symbol->string name)
                                           " used outside of ~seq-no-order")))
      (apply (parameter-name) args))))

(define-dynamic-accumulator-parameter eh-post-accumulate eh-post-accumulate!)
(define-dynamic-accumulator-parameter eh-post-group eh-post-group!)

(define-for-syntax clause-counter (make-parameter #f))
(define-for-syntax (get-new-clause!)
  (string->symbol (format "clause~a" ((clause-counter)))))