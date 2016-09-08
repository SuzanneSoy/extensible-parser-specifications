#lang racket

(require syntax/parse
         extensible-parser-specifications
         phc-toolkit/untyped
         rackunit)

(define-syntax-class abc-order
  (pattern
   {~no-order
    {~optional {~order-point a-point #:a
                 {~post-fail "#:a must appear after #:b"
                             #:when (order-point> a-point b-point)}}}
    {~optional {~order-point b-point #:b}}
    {~optional {~order-point c-point #:c}}}))

(define-syntax-rule (check-parse-abc stx)
  (check-true (syntax-parse stx
                [:abc-order #t]
                [_ #f])))

(define-syntax-rule (check-fail-abc stx exn)
  (check-exn exn
             (λ ()
               (syntax-parse stx
                 [:abc-order 'ok]))))

(check-parse-abc #'(#:a))
(check-parse-abc #'(#:b))
(check-parse-abc #'(#:c))
(check-parse-abc #'(#:a #:b))
(check-parse-abc #'(#:c #:a))
(check-parse-abc #'(#:a #:c))
(check-parse-abc #'(#:c #:b))
(check-parse-abc #'(#:b #:c))
(check-parse-abc #'(#:c #:a #:b))
(check-parse-abc #'(#:a #:c #:b))
(check-parse-abc #'(#:a #:b #:c))
(check-fail-abc #'(#:b #:a) #px"#:a must appear after #:b")
(check-fail-abc #'(#:c #:b #:a) #px"#:a must appear after #:b")
(check-fail-abc #'(#:b #:c #:a) #px"#:a must appear after #:b")
(check-fail-abc #'(#:b #:a #:c) #px"#:a must appear after #:b")
(check-fail-abc #'(#:a #:a) #px"unexpected term")
(check-fail-abc #'(#:c #:c) #px"unexpected term")