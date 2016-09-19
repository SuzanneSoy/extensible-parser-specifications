#lang racket/base

(require generic-syntax-expanders
         "private/parameters.rkt"
         "private/no-order.rkt"
         "private/post.rkt"
         "private/global.rkt"
         "private/optional.rkt"
         "private/mixin.rkt"
         "private/try-attribute.rkt"
         (for-template "private/define-syntax+simple-api.rkt")
         syntax/parse)

;; from syntax/parse, so that define-eh-alternative-mixin can recognize uses of
;; (pattern …)
(provide pattern)

(provide #;define-splicing-syntax-class-with-eh-mixins
         #;define-syntax-class-with-eh-mixins
         define-eh-alternative-mixin
         (expander-out eh-mixin)
         ~seq-no-order
         ~no-order
         ~order-point
         order-point<
         order-point>
         ~mixin
         ~post-check
         ~post-fail
         ~named-seq
         ~nop
         ~optional/else
         ~global-or
         ~global-and
         ~global-counter
         aggregate-global-or
         aggregate-global-and
         aggregate-global-counter
         (for-template define-syntax/parse+simple)
         define/syntax-parse+simple
         try-attribute)
