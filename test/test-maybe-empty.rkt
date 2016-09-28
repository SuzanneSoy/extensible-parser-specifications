#lang racket

(require extensible-parser-specifications
         racket/require
         syntax/parse
         (subtract-in syntax/stx phc-toolkit/untyped)
         rackunit
         racket/format
         phc-toolkit/untyped
         (for-syntax syntax/parse
                     syntax/stx
                     racket/format))

(check-equal? (syntax-parse #'()
                [{~no-order (~maybe/empty {~seq τᵢ ... {~lift-rest τ-rest}})}
                 (syntax->datum #'(#:rest τ-rest #:τᵢ τᵢ …))])
              '(#:rest () #:τᵢ))

(check-equal? (syntax-parse #'a
                 [{~no-order (~maybe/empty {~seq τᵢ ... {~lift-rest τ-rest}})}
                  (syntax->datum #'(#:rest τ-rest #:τᵢ τᵢ …))])
              '(#:rest a #:τᵢ))

(check-equal? (syntax-parse #'(a)
                 [{~no-order (~maybe/empty {~seq τᵢ ... {~lift-rest τ-rest}})}
                  (syntax->datum #'(#:rest τ-rest #:τᵢ τᵢ …))])
              '(#:rest () #:τᵢ a))

(check-equal? (syntax-parse #'(a . b)
                 [{~no-order (~maybe/empty {~seq τᵢ ... {~lift-rest τ-rest}})}
                  (syntax->datum #'(#:rest τ-rest #:τᵢ τᵢ …))])
              '(#:rest b #:τᵢ a))

(check-equal? (syntax-parse #'(a b)
                 [{~no-order (~maybe/empty {~seq τᵢ ... {~lift-rest τ-rest}})}
                  (syntax->datum #'(#:rest τ-rest #:τᵢ τᵢ …))])
              '(#:rest () #:τᵢ a b))

(check-equal? (syntax-parse #'(a b . c)
                 [{~no-order (~maybe/empty {~seq τᵢ ... {~lift-rest τ-rest}})}
                  (syntax->datum #'(#:rest τ-rest #:τᵢ τᵢ …))])
              '(#:rest c #:τᵢ a b))