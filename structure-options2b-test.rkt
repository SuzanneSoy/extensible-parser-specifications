#lang racket

(require "structure-options2b.rkt"
         racket/require
         syntax/parse
         (subtract-in syntax/stx phc-toolkit/untyped)
         rackunit
         racket/format
         phc-toolkit/untyped
         (for-syntax syntax/parse
                     syntax/stx
                     racket/format))

(check-equal? (syntax-parse #'(1 "ab" #:kw "ab" 3 4 5)
                [({~no-order {~once {~global-counter cnt 'occurrencea #:kw}}
                             {~global-counter cnt 'occurrenceb :number}
                             "ab"})
                 (attribute cnt)])
              5)

(check-equal? (syntax-parse #'(1 "ab" #:kw "ab" 3 4 5)
                [({~no-order {~once {~global-or kw-or-number #t #:kw}}
                             {~global-or kw-or-number #t :number}
                             "ab"})
                 (attribute kw-or-number)])
              #t)

(check-equal? (syntax-parse #'(1 "ab" "ab" 3 4 5)
                [({~no-order {~optional {~global-or kw #t #:kw}}
                             {~global-or kw #f :number}
                             "ab"})
                 (attribute kw)])
              #f)

(check-equal? (syntax-parse #'(1 "ab" #:kw "ab" 3 4 5)
                [({~no-order {~optional {~global-and kw-and-not-number #t #:kw}}
                             {~global-and kw-and-not-number #f :number}
                             "ab"})
                 (attribute kw-and-not-number)])
              #f)

(check-equal? (syntax-parse #'("ab" #:kw "ab")
                [({~no-order {~optional {~global-and kw-and-not-number #t #:kw}}
                               {~global-and kw-and-not-number #f :number}
                               "ab"})
                 (attribute kw-and-not-number)])
              #t)