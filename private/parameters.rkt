#lang racket/base

(require (for-syntax racket/base))

(provide (for-syntax eh-pre-accumulate
                     eh-pre-accumulate!
                     eh-post-accumulate
                     eh-post-accumulate!
                     eh-post-group
                     eh-post-group!
                     clause-counter
                     get-new-clause!
                     is-clause-id-sym?
                     lift-rest
                     lift-rest!))

(define-syntax-rule (define-dynamic-accumulator-parameter parameter-name name!)
  (begin
    (define-for-syntax parameter-name (make-parameter #f))
    (define-for-syntax (name! name . args)
      (unless (parameter-name)
        (raise-syntax-error name
                            (string-append (symbol->string name)
                                           " used outside of ~seq-no-order")))
      (apply (parameter-name) args))))

(define-dynamic-accumulator-parameter eh-pre-accumulate eh-pre-accumulate!)
(define-dynamic-accumulator-parameter eh-post-group eh-post-group!)
(define-dynamic-accumulator-parameter eh-post-accumulate eh-post-accumulate!)
(define-dynamic-accumulator-parameter lift-rest lift-rest!)

;; This is a crude hack.
(define-for-syntax (is-clause-id-sym? id-sym)
  (and (symbol? id-sym)
       (regexp-match #px"^ -clause-.* $" (symbol->string id-sym))))

(define-for-syntax clause-counter (make-parameter #f))
(define-for-syntax (get-new-clause!)
  (unless clause-counter
    (error "Use get-new-clause! within (parameterize ([clause-counter …]) …)"))
  (datum->syntax #'here
                 ;; keep the spaces, they allow us to recognize clauses later.
                 (string->symbol (format " -clause-~a " ((clause-counter))))))
