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

(define-syntax/parse (define-~global global-name (~optional default) f)
  (define use-default-v? (syntax-e #'default-v?))
  (template
   (define-eh-mixin-expander global-name
     (syntax-parser
       [(_ (?? (~or [name v] (~and name (~bind [v default])))
               [name v])
           . pat)
        (define/with-syntax clause-value (get-new-clause!))
        (eh-post-group! '~global-name
                        #'name ;(syntax-e #'name)
                        #'f
                        #'clause-value)
        ;; protect the values inside an immutable box, so that a #f can be
        ;; distinguished from a failed match.
        #'(~and (~bind [clause-value (box-immutable v)])
                . pat)]))))

(define (aggregate-global-or . bs)
  (ormap unbox ;; remove the layer of protection
         (filter identity ;; remove failed bindings
                 (flatten bs)))) ;; don't care about ellipsis nesting
(define-~global ~global-or #'#t aggregate-global-or)

(define (aggregate-global-and . bs)
  (andmap unbox ;; remove the layer of protection
          (cons (box-immutable 'none) ;; default value when no bindings matched
                (filter identity ;; remove failed bindings
                        (flatten bs))))) ;; don't care about ellipsis nesting
(define-~global ~global-and aggregate-global-and)

(define (aggregate-global-counter . bs)
  (length (filter identity ;; remove failed bindings
                  (flatten bs)))) ;; don't care about ellipsis nesting
(define-~global ~global-counter #''occurrence aggregate-global-counter)

