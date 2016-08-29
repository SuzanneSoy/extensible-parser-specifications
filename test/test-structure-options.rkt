#lang racket

(require extensible-parser-specifications
         racket/require
         syntax/parse
         rackunit
         racket/format
         phc-toolkit/untyped
         (for-syntax syntax/parse
                     racket/format))

(provide structure-kw-instance-or-builder-mixin
         structure-kw-predicate-mixin
         structure-kw-fields-mixin
         structure-kw-all-mixin
         structure-kws)

(define-eh-alternative-mixin structure-kw-instance-or-builder-mixin
  (pattern
   {~optional {~and instance-or-builder
                    {~or {~global-or instance #:instance}
                         {~global-or builder #:builder}}}
              #:name "either #:instance or #:builder"}))

(define-eh-alternative-mixin structure-kw-predicate-mixin
  (pattern {~optional {~seq #:? predicate:id}
                      #:name "#:? predicate"}))

(define-and-for-syntax no-values-err
  (~a "The #:instance keyword implies the use of [field value],"
      " [field : type value] or [field value : type]."))

(define-and-for-syntax values-err
  (~a "The #:builder keyword implies the use of [field], field"
      " or [field : type]."))

(define-and-for-syntax empty-err
  (~a "If no fields are specified, then either #:builder or #:instance"
      " must be present"))

(define-eh-alternative-mixin structure-kw-fields-mixin
  (pattern
   {~optional/else
    {~or {~seq {~or-bug [field:id] field:id} …+
               {~global-or builder}
               {~global-or no-types}
               {~post-fail no-values-err #:when (attribute instance)}}
         {~seq [field:id : type] …+
               {~global-or builder}
               {~global-or types}
               {~post-fail no-values-err #:when (attribute instance)}}
         {~seq [field:id value:expr] …+
               {~global-or instance}
               {~global-or no-types}
               {~post-fail values-err #:when (attribute builder)}}
         {~seq {~or-bug [field:id value:expr : type]
                        [field:id : type value:expr]}
               …+
               {~global-or instance}
               {~global-or types}
               {~post-fail values-err #:when (attribute builder)}}}
    #:defaults ([(field 1) (list)]
                [(value 1) (list)]
                [(type 1) (list)])
    #:else-post-fail empty-err #:when (and (not (attribute builder))
                                           (not (attribute instance)))
    #:name (~a "field or [field] or [field : type] for #:builder,"
               " [field value] or [field : type value]"
               " or [field value : type] for #:instance")}))

(define-eh-alternative-mixin structure-kw-all-mixin
  (pattern {~or {structure-kw-instance-or-builder-mixin}
                {structure-kw-predicate-mixin}
                {structure-kw-fields-mixin}}))

;; ---------

(define-splicing-syntax-class structure-kws
  (pattern {~seq-no-order {structure-kw-all-mixin}}))

(check-equal? (syntax-parse #'(#:instance #:? p)
                [(:structure-kws)
                 (list* (attribute instance)
                        (syntax->datum
                         #'(instance-or-builder
                            predicate
                            [field ...]
                            [value ...])))])
              '(#t #:instance p [] []))

(check-equal? (syntax-parse #'(#:builder)
                [(k:structure-kws)
                 (list* (attribute k.builder)
                        (syntax->datum
                         #'(k.instance-or-builder [k.field ...])))])
              '(#t #:builder []))

(test-exn
 "Check that () is rejected, as it has neither #:instance nor #:builder"
 (regexp (regexp-quote empty-err))
 (λ ()
   (syntax-parse #'()
     [(:structure-kws) #'()])))

(test-exn
 "Check that (#:instance [f1] [f2]) is rejected, as #:instance conflicts with
builder-style field declarations"
 (regexp (regexp-quote no-values-err))
 (λ ()
   (syntax-parse #'(#:instance [f1] [f2])
     [(:structure-kws) #'([field ...] instance)])))

(check-equal? (syntax-parse #'(#:builder #:? p [f1] [f2])
                [(:structure-kws) (list* (attribute builder)
                                         (syntax->datum #'([field ...])))])
              '(#t [f1 f2]))

(check-equal?  (syntax-parse #'([f1] [f2] #:? p)
                 [(:structure-kws) (cons (attribute builder)
                                         (syntax->datum #'([field ...])))])
               '(#t [f1 f2]))

;; This one is appropriately rejected
(check-exn #px"unexpected term"
           (λ ()
             (syntax-parse #'(#:instance #:a)
               [(:structure-kws) 'err])))

(define instance-or-builder?
  (syntax-parser [(:structure-kws) (list (attr instance) (attr builder))]))

(check-equal? '(#t #f) (instance-or-builder? #'(#:instance)))
(check-equal? '(#f #t) (instance-or-builder? #'(#:builder)))
(check-equal? '(#f #t) (instance-or-builder? #'(f1)))
(check-equal? '(#f #t) (instance-or-builder? #'([f1])))
(check-equal? '(#f #t) (instance-or-builder? #'([f1] f2)))
(check-equal? '(#f #t) (instance-or-builder? #'([f1 : type])))
(check-equal? '(#t #f) (instance-or-builder? #'([f1 value])))
(check-equal? '(#t #f) (instance-or-builder? #'([f1 : type value])))
(check-equal? '(#t #f) (instance-or-builder? #'([f1 value : type])))
(check-equal? '(#f #t) (instance-or-builder? #'(f1 #:builder)))
(check-equal? '(#f #t) (instance-or-builder? #'([f1] #:builder)))
(check-equal? '(#f #t) (instance-or-builder? #'([f1] f2 #:builder)))
(check-equal? '(#f #t) (instance-or-builder? #'([f1 : type] #:builder)))
(check-equal? '(#t #f) (instance-or-builder? #'([f1 value] #:instance)))
(check-equal? '(#t #f) (instance-or-builder? #'([f1 : type value] #:instance)))
(check-equal? '(#t #f) (instance-or-builder? #'([f1 value : type] #:instance)))

;; TODO: use (reified-syntax-class-attributes r) to make a simplified version
;; of a macro, which just accepts all the attributes. Another macro can
;; then forward all the attributes at once, with minimal meta-level 1 cost
;; (obviously, constructing the wrappers etx. will have some metal-level 2 cost)
;;
;; Wrapper:
;; (define-syntax (real-macro-name stx)
;;   (syntax-parse stx
;;     [(~reflect whole some-reified-splicing-syntax-class)
;;      (simplified-macro-implementation (attribute attr0) ...)]))
;; Implementation
;; (define-for-syntax (simplified-macro-implementation val0 ...)
;;   (syntax-parse #'dummy
;;     [(~bind [(attr0 depth) val0] ...)
;;      body]))
;;
;; For speed, we could just copy the whole implementation in real-macro-name's
;; definition, instead of calling simplified-macro-implementation.