#lang racket

(require "no-order.rkt"
         (for-syntax syntax/parse
                     racket/syntax))

(provide ~mixin)

(define-eh-mixin-expander ~mixin
  (syntax-parser
    [(_ (~var mixin (static eh-mixin-expander? "an eh-mixin expander")))
     (with-disappeared-uses
      (begin
        (record-disappeared-uses #'mixin)
        #'(mixin)))]))