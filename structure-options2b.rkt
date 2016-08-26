#lang racket

(require syntax/parse
         syntax/parse/experimental/eh
         generic-syntax-expanders
         phc-toolkit/untyped
         (for-syntax syntax/parse
                     racket/syntax
                     phc-toolkit/untyped
                     racket/list
                     generic-syntax-expanders
                     racket/contract))

(provide ;define-splicing-syntax-class-with-eh-mixins
 ;define-syntax-class-with-eh-mixins
 define-eh-alternative-mixin
 (expander-out eh-mixin)
 ~no-order
 ~post-check
 ~post-fail
 ~nop
 ~optional/else
 ~global-or
 ~global-and
 ~global-counter)

;; ------------
;; eh-mixin — TODO: move to phc-toolkit once the PR #7 for reqprov in
;; generic-syntax-expander is merged.

(define-expander-type eh-mixin)

(define-syntax-rule (define-dynamic-accumulator-parameter parameter-name name!)
  (begin
    (define-for-syntax parameter-name (make-parameter #f))
    (define-for-syntax (name! name . args)
      (unless (parameter-name)
        (raise-syntax-error name
                            (string-append (symbol->string name)
                                           " used outside of ~no-order")))
      (apply (parameter-name) args))))

(define-dynamic-accumulator-parameter eh-post-accumulate eh-post-accumulate!)
(define-dynamic-accumulator-parameter eh-pre-declarations eh-pre-declare!)

;; ----

(define-for-syntax clause-counter (make-parameter #f))
(define-for-syntax (get-new-clause!)
  (string->symbol (format "clause~a" ((clause-counter)))))

(define-syntax define-eh-alternative-mixin
  (syntax-parser
    [(_ name ((~literal pattern) pat) ...)
     (let ()
       (define/with-syntax mixin (format-id #'name "~a-mixin" #'name))
       (define-temp-ids "~a/clause" (pat ...))
       #'(define-eh-mixin-expander mixin
           (λ (_)
             (quote-syntax (~or pat ...)))))]))

;; ----------

(define-for-syntax (inline-or stx)
  (syntax-case stx ()
    [(o . rest)
     (and (identifier? #'o) (free-identifier=? #'o #'~or))
     (apply append (stx-map inline-or #'rest))]
    [x (list #'x)]))

;; TODO: ~no-order should also be a eh-mixin-expander, so that when there are
;; nested ~no-order, the ~post-fail is caught by the nearest ~no-order.
(define-syntax ~no-order
  (pattern-expander
   (λ (stx)
     (syntax-case stx ()
       [(self pat ...)
        (let ()
          (define counter 0)
          (define (increment-counter)
            (begin0 counter
                    (set! counter (add1 counter))))
          (define post-acc '())
          (define (add-to-post! v) (set! post-acc (cons v post-acc)))
          ;; pre-acc gathers some bindings that have to be pre-declared
          (define pre-acc (make-hash))
          (define/contract (add-to-pre! s v) (-> symbol? any/c identifier?)
            (define not-found (gensym))
            (define ref (hash-ref pre-acc s #f))
            (if ref
                (car ref)
                (let ([id (datum->syntax (syntax-local-introduce #'here) s)])
                  (hash-set! pre-acc s (cons id v))
                  id)))
          ;(define-values (pre-acc add-to-pre) (make-mutable-accumulator))
          (define alts
            (parameterize ([eh-post-accumulate add-to-post!]
                           [eh-pre-declarations add-to-pre!]
                           [clause-counter increment-counter])
              (inline-or (expand-all-eh-mixin-expanders #'(~or pat ...)))))
          (define pre-acc-bindings (hash-map pre-acc
                                             (λ (s bv) #`(define . #,bv))))
          #`(~delimit-cut
             (~and (~do #,@pre-acc-bindings)
                   (~seq (~or . #,alts) (... ...))
                   ~!
                   #,@post-acc)))]))))

(define-syntax ~nop
  (pattern-expander
   (λ/syntax-case (_) () #'(~do))))

(define-eh-mixin-expander ~post-check
  (λ (stx)
    (syntax-case stx ()
      [(_ pat post)
       (begin (eh-post-accumulate! '~post-check #'post)
              #'pat)]
      [(_ post)
       (begin (eh-post-accumulate! '~post-check #'post)
              #'(~nop))])))

(define-for-syntax (post-fail stx)
  (syntax-case stx ()
    [(_ message #:when condition)
     (begin
       (define/with-syntax clause-present (get-new-clause!))
       (eh-post-accumulate! '~post-fail
                            #`(~fail #:when (and (attribute clause-present)
                                                 condition)
                                     message))
       #'(~bind [clause-present #t]))]
    [(self #:when condition message)
     (post-fail #'(self message #:when condition))]))

(define-eh-mixin-expander ~post-fail post-fail)

(define-syntax ~mutex
  (pattern-expander
   (λ (stx)
     (syntax-case stx ()
       [(self (mutex:id ...) pat ...)
        #'(???)]))))

(define-syntax-rule (define-~global ~global-name init f)
  (define-eh-mixin-expander ~global-name
    (λ/syntax-case (_ name v pat) ()
      (eh-pre-declare! '~bool-or (syntax-e #'name) init)
      #`(~and (~do (define tmp name))
              (~do (define name (#,f tmp v)))
              pat))))

(define-~global ~global-or #f (λ (acc v) (or acc v)))
(define-~global ~global-and #t (λ (acc v) (and acc v)))
(define-~global ~global-counter 0 add1)

(define-eh-mixin-expander ~optional/else
  (syntax-parser
    [(_ pat
        (~optional (~seq #:defaults (default-binding ...))
                   #:defaults ([(default-binding 1) (list)]))
        (~seq #:else-post-fail (~or (~seq message #:when condition)
                                    (~seq #:when condition message)))
        ...
        (~optional (~seq #:name name)))
     #:with clause-whole (get-new-clause!)
     #:with clause-present (get-new-clause!)
     (for ([message (in-syntax #'(message ...))]
           [condition (in-syntax #'(condition ...))])
       (eh-post-accumulate! '~optional/else
                            #`(~fail #:when (and (eq? (attr clause-present) 0)
                                                 #,condition)
                                     #,message)))
     #`(~optional (~and pat
                        ;(~seq clause-whole (... ...))
                        ;; can't use #f, because of the bug
                        ;; https://github.com/racket/racket/issues/1437
                        (~bind [clause-present 1]))
                  #:defaults (default-binding ...
                               ;[(clause-whole 1) #'()]
                               [clause-present 0])
                  #,@(if (attribute name) #'(#:name name) #'()))]))