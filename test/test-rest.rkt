#lang racket

(require extensible-parser-specifications
         extensible-parser-specifications/private/no-order
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
 (syntax-parse #'(1 "ab" . #:kw)
   [(~no-order {~or {~lift-rest {~and k #:kw}}
                    {~once n:nat}}
               {~once s:str})
    (syntax->datum #'(k n s))])
 '(#:kw 1 "ab"))


(check-equal?
 (syntax-parse #'(1 "ab" . #:kw)
   [(~no-order {~lift-rest {~and k #:kw}}
               {~once n:nat}
               {~once s:str})
    (syntax->datum #'(k n s))])
 '(#:kw 1 "ab"))

(check-equal?
 (syntax-parse #'(1 "ab" . #:kw)
   [(~no-order {~once {~and n:nat
                            {~lift-rest {~and k #:kw}}}}
               {~once s:str})
    (syntax->datum #'(k n s))])
 '(#:kw 1 "ab"))

(check-equal?
 (syntax-parse #'(1 "ab" . #:kw)
   [(~no-order {~once n:nat}
               {~lift-rest {~and k #:kw}}
               {~once s:str})
    (syntax->datum #'(k n s))]
   [_ #f])
 '(#:kw 1 "ab"))

(test-begin
 "Exactly the same as above, but with the post-fail"
 (check-false
  (syntax-parse #'(1 "ab" . #:kw)
    [(~no-order {~once n:nat}
                {~lift-rest {~and k #:kw
                                  {~post-fail "e" #:when (= (syntax-e #'n) 1)}}}
                {~once s:str})
     (syntax->datum #'(k n s))]
    [_ #f])))

(test-begin
 "Exactly the same as above, but with a different value (2 instead of 1)"
 (check-equal?
  (syntax-parse #'(2 "ab" . #:kw)
    [(~no-order {~once n:nat}
                {~lift-rest {~and k #:kw
                                  {~post-fail "e" #:when (= (syntax-e #'n) 1)}}}
                {~once s:str})
     (syntax->datum #'(k n s))]
    [_ #f])
  '(#:kw 2 "ab")))

(define p
  (syntax-parser
    [(~no-order {~and {~literal x}
                      {~lift-rest rn:nat}
                      {~lift-rest ri:id}}
                {~and {~literal y}
                      {~lift-rest rs:str}
                      {~lift-rest rj:id}})
     'match]))

(check-equal? (p #'(x . 1)) 'match)
(check-equal? (p #'(x . z)) 'match)
(check-equal? (p #'(y . "a")) 'match)
(check-equal? (p #'(y . z)) 'match)
(check-equal? (p #'(x y . 1)) 'match)
(check-exn #px"more than one of the lifted rest patterns matched"
           (λ () (p #'(x y . z))))