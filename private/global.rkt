#lang racket/base

(require racket/function
         racket/list
         syntax/parse
         phc-toolkit/untyped
         (for-syntax racket/base
                     syntax/parse
                     syntax/parse/experimental/template
                     racket/syntax
                     phc-toolkit/untyped)
         "parameters.rkt"
         "no-order.rkt")

(provide ~global-or
         ~global-and
         ~global-counter
         aggregate-global-or
         aggregate-global-and
         aggregate-global-counter)

(define-for-syntax (make-~global f [default #f])
  (syntax-parser
    [(_ (~or [name v] (~and name
                            (~fail #:unless default)
                            (~bind [v default])))
        pat ...)
     #:with clause-value (get-new-clause!)
     (eh-post-group! '~global-name
                     #'name
                     f
                     #'clause-value)
     ;; protect the values inside an immutable box, so that a #f can be
     ;; distinguished from a failed match.
     #'(~and pat ...
             (~bind [clause-value (box-immutable v)]))]))

(define (aggregate-global-or . bs)
  (ormap unbox ;; remove the layer of protection
         (filter identity ;; remove failed bindings
                 (flatten bs)))) ;; don't care about ellipsis nesting
(define-eh-mixin-expander ~global-or
  (make-~global #'aggregate-global-or #'#t))

(define (aggregate-global-and . bs)
  (andmap unbox ;; remove the layer of protection
          (cons (box-immutable 'none) ;; default value when no bindings matched
                (filter identity ;; remove failed bindings
                        (flatten bs))))) ;; don't care about ellipsis nesting
(define-eh-mixin-expander ~global-and
  (make-~global #'aggregate-global-and))

(define (aggregate-global-counter . bs)
  (apply + (filter identity ;; remove failed bindings
                   (flatten bs)))) ;; don't care about ellipsis nesting
(define-eh-mixin-expander ~global-counter
  (make-~global #'aggregate-global-counter #'+1))

