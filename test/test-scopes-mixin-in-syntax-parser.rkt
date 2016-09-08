#lang racket
(require phc-toolkit/untyped
         extensible-parser-specifications
         syntax/parse
         rackunit)

(define-eh-alternative-mixin props-mixin
  (pattern
   (~optional (~seq #:foo bar))))

(define test
  (syntax-parser
    [(~no-order {~mixin props-mixin})
     (attribute bar)]))

(test-equal?
 "Without the bugfix, the pattern variable \"bar\" above had the wrong scopes,
and couldn't be used with (attribute bar), and #'bar just gave #'bar instead of
producing #'42"
 (syntax-e (test #'(#:foo bar)))
 42)