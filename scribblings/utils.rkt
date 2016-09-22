#lang racket

(require scribble/manual
         scribble/example)

(provide (all-defined-out))

(define ntax-pattern (tech #:doc '(lib "syntax/scribblings/syntax.scrbl")
                           #:key "syntax pattern"
                           "syntax-pattern"))

(define -alternative-mixin (tech #:key "eh-alternative mixin"
                                 #:doc '(lib "extensible-parser-specifications/scribblings/extensible-parser-specifications.scrbl")
                                 "eh-alternative-mixin"))

(define tribute-name (tech #:doc '(lib "syntax/scribblings/syntax.scrbl")
                           #:key "attribute"
                           "attribute-name"))

(define A-patte (tech #:doc '(lib "syntax/scribblings/syntax.scrbl")
                      #:key "action pattern"
                      "A-pattern"))

(define make-evaluator
  (make-eval-factory '(syntax/parse
                       extensible-parser-specifications)))
