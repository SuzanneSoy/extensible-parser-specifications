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

(check-equal?
 (syntax-parse #'(1 "ab" #:kw "ab" 3 4 5)
   [({~seq-no-order {~once {~global-counter [cnt 1] #:kw}}
                    {~global-counter [cnt 1] :number}
                    "ab"})
    (attribute cnt)])
 5)

(check-equal?
 (syntax-parse #'(1 "ab" #:kw "ab" 3 4 5)
   [({~seq-no-order {~once {~global-or kw-or-number #:kw}}
                    {~global-or kw-or-number :number}
                    "ab"})
    (attribute kw-or-number)])
 #t)

(check-equal?
 (syntax-parse #'(1 "ab" "ab" 3 4 5)
   [({~seq-no-order {~optional {~global-or [kw #t] #:kw}}
                    {~global-or [kw #f] :number}
                    "ab"})
    (attribute kw)])
 #f)

(check-equal?
 (syntax-parse #'(1 "ab" #:kw "ab" 3 4 5)
   [({~seq-no-order {~optional {~global-and [kw-not-number #t] #:kw}}
                    {~global-and [kw-not-number #f] :number}
                    "ab"})
    (attribute kw-not-number)])
 #f)

(check-equal?
 (syntax-parse #'("ab" "ab")
   [({~seq-no-order {~optional {~global-and [kw-not-number #t] #:kw}}
                    {~global-and [kw-not-number #f] :number}
                    "ab"})
    (attribute kw-not-number)])
 ;; (and) of nothing is #t, but we provide a 'none value
 ;; for this special case
 'none)

(check-equal?
 (syntax-parse #'("ab" #:kw "ab")
   [({~seq-no-order {~optional {~global-and [kw-not-number #t] #:kw}}
                    {~global-and [kw-not-number #f] :number}
                    "ab"})
    (attribute kw-not-number)])
 #t)

;; Tests from the documentation:

(check-equal?
 (syntax-parse #'(1 ya (2 #f 3) 4 yb (5 #f 6) yc 7)
   [(~no-order {~and x:id {~global-or [g (syntax-e #'x)]}}
               {~global-or [g (syntax-e #'y)] y:number}
               ({~global-or [g (syntax-e #'z)] (~and z (~or :number #f))}
                …)
               {~global-or [g (syntax-e #'w)] w:str})
    (attribute g)])
 'ya)

(check-equal?
 (syntax-parse #'(1 ya (2 3) 4 yb (5 6) yc 7)
   [(~no-order {~and x:id {~global-and [g (syntax-e #'x)]}}
               {~global-and [g (syntax-e #'y)] y:number}
               ({~global-and [g (syntax-e #'z)] (~and z :number)}
                …)
               {~global-and [g (syntax-e #'w)] w:str})
    (attribute g)])
 6)