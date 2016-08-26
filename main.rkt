#lang racket

(require syntax/parse
         syntax/parse/experimental/eh
         generic-syntax-expanders
         phc-toolkit/untyped
         (for-syntax syntax/parse
                     syntax/parse/experimental/template
                     racket/syntax
                     phc-toolkit/untyped
                     racket/list
                     generic-syntax-expanders
                     racket/function
                     racket/pretty))

(provide ;define-splicing-syntax-class-with-eh-mixins
 ;define-syntax-class-with-eh-mixins
 define-eh-alternative-mixin
 (expander-out eh-mixin)
 ~seq-no-order
 ~post-check
 ~post-fail
 ~nop
 ~optional/else
 ~global-or
 ~global-and
 ~global-counter
 aggregate-global-or
 aggregate-global-and
 aggregate-global-counter)

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
                                           " used outside of ~seq-no-order")))
      (apply (parameter-name) args))))

(define-dynamic-accumulator-parameter eh-post-accumulate eh-post-accumulate!)
(define-dynamic-accumulator-parameter eh-post-group eh-post-group!)

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

;; TODO: ~seq-no-order should also be a eh-mixin-expander, so that when there
;; are nested ~seq-no-order, the ~post-fail is caught by the nearest
;; ~seq-no-order.
(define-syntax ~seq-no-order
  (pattern-expander
   (λ (stx)
     (syntax-case stx ()
       [(self pat ...)
        ((λ (x) #;(pretty-write (syntax->datum x)) x)
         (let ()
           (define counter 0)
           (define (increment-counter)
             (begin0 counter
                     (set! counter (add1 counter))))
           ;; post-acc gathers some a-patterns which will be added after the
           ;; (~seq (~or ) ...)
           (define post-acc '())
           (define (add-to-post! v) (set! post-acc (cons v post-acc)))
           ;; post-groups-acc gathers some attributes that have to be grouped
           (define post-groups-acc '())
           (define (add-to-post-groups! . v)
             (set! post-groups-acc (cons v post-groups-acc)))
           ;; expand EH alternatives:
           (define alts
             (parameterize ([eh-post-accumulate add-to-post!]
                            [eh-post-group add-to-post-groups!]
                            [clause-counter increment-counter])
               (inline-or (expand-all-eh-mixin-expanders #'(~or pat ...)))))
           (define post-group-bindings
             (for/list ([group (group-by car
                                         post-groups-acc
                                         free-identifier=?)])
               ;; each item in `group` is a four-element list:
               ;; (list result-id aggregate-function attribute)
               (define/with-syntax name (first (car group))
                 #;(syntax-local-introduce
                    (datum->syntax #'here
                                   (first (car group)))))
               (define/with-syntax f (second (car group)))
               #`[name (f . #,(map (λ (i) #`(attribute #,(third i)))
                                   group))]))
           #`(~delimit-cut
              (~and (~seq (~or . #,alts) (... ...))
                    ~!
                    (~bind #,@post-group-bindings)
                    #,@post-acc))))]))))

(define-syntax ~no-order
  (pattern-expander
   (λ/syntax-case (_ . rest) ()
     #'({~seq-no-order . rest}))))

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

(define-syntax/parse (define-~global global-name (~optional default) f)
  (define use-default-v? (syntax-e #'default-v?))
  (template
   (define-eh-mixin-expander global-name
     (syntax-parser
       [(_ (?? (~or [name v] (~and name (~bind [v default])))
               [name v])
           . pat)
        (define/with-syntax clause-value (get-new-clause!))
        (eh-post-group! '~global-name
                        #'name ;(syntax-e #'name)
                        #'f
                        #'clause-value)
        ;; protect the values inside an immutable box, so that a #f can be
        ;; distinguished from a failed match.
        #'(~and (~bind [clause-value (box-immutable v)])
                . pat)]))))

(define (aggregate-global-or . bs)
  (ormap unbox ;; remove the layer of protection
         (filter identity ;; remove failed bindings
                 (flatten bs)))) ;; don't care about ellipsis nesting
(define-~global ~global-or #'#t aggregate-global-or)

(define (aggregate-global-and . bs)
  (andmap unbox ;; remove the layer of protection
          (cons (box-immutable 'none) ;; default value when no bindings matched
                (filter identity ;; remove failed bindings
                        (flatten bs))))) ;; don't care about ellipsis nesting
(define-~global ~global-and aggregate-global-and)

(define (aggregate-global-counter . bs)
  (length (filter identity ;; remove failed bindings
                  (flatten bs)))) ;; don't care about ellipsis nesting
(define-~global ~global-counter #''occurrence aggregate-global-counter)

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