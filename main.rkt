#lang racket/base

(require generic-syntax-expanders
         "private/parameters.rkt"
         "private/no-order.rkt"
         "private/before-after.rkt"
         "private/pre.rkt"
         "private/post.rkt"
         "private/global.rkt"
         "private/optional.rkt"
         "private/mixin.rkt"
         "private/try-attribute.rkt"
         "private/nop.rkt"
         (for-template "private/define-syntax+simple-api.rkt")
         syntax/parse)

;; re-provide pattern as provided by syntax/parse, so that
;; define-eh-alternative-mixin can recognize uses of (pattern …)
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
         try-order-point<
         try-order-point>
         ~before
         ~after
         ~try-before
         ~try-after
         ~lift-rest
         ~as-rest
         ~mixin
         ~post-check
         ~post-fail
         ~maybe/empty
         ~named-seq
         ~nop
         ~optional/else
         ~global-or
         ~global-and
         ~global-counter
         (for-template define-syntax/parse+simple)
         define/syntax-parse+simple
         try-attribute
         if-attribute)
