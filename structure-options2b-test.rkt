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

(syntax-parse #'(1 #:kw 3)
  [{~no-order {~once {~global-counter #:kw }} }
