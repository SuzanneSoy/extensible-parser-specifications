#lang racket

(require syntax/parse
         syntax/parse/experimental/eh
         generic-syntax-expanders
         (for-syntax syntax/parse
                     racket/syntax
                     phc-toolkit/untyped
                     racket/list
                     generic-syntax-expanders
                     racket/pretty))

(provide ;define-splicing-syntax-class-with-eh-mixins
 ;define-syntax-class-with-eh-mixins
 define-eh-alternative-mixin
 (expander-out eh-mixin)
 ~no-order
 ~post-check
 ~post-fail)

;; ------------
;; eh-mixin — TODO: move to phc-toolkit once the PR #7 for reqprov in
;; generic-syntax-expander is merged.

(define-expander-type eh-mixin)

(define-for-syntax eh-post-accumulate (make-parameter #f))
#;(define-for-syntax current-no-order-clause (make-parameter #f))

(define-syntax define-eh-alternative-mixin
  (syntax-parser
    [(_ name ((~literal pattern) pat) ...)
     (let ()
       (define/with-syntax mixin (format-id #'name "~a-mixin" #'name))
       (define-temp-ids "~a/clause" (pat ...))
       #'(define-eh-mixin-expander mixin
           (λ (_)
             (quote-syntax (~or pat ...))
             #;#`(~or #,(parameterize ([current-no-order-clause #'pat/clause])
                          (quote-syntax pat))
                      ...))))]))

;; ----------

(define-for-syntax (inline-or stx)
  (syntax-case stx ()
    [(o . rest)
     (and (identifier? #'o) (free-identifier=? #'o #'~or))
     (apply append (stx-map inline-or #'rest))]
    [x (list #'x)]))

#;(define-for-syntax (expand-no-order-clauses/tree x)
    (cond
      [(syntax? x) (datum->syntax x
                                  (expand-no-order-clauses/tree (syntax-e x))
                                  x
                                  x)]))

#;(define-for-syntax (expand-no-order-clauses stx)
    (syntax-case stx (~or)
      [(~or pat ...) (append-map expand-no-order-clauses
                                 (syntax->list #'(pat ...)))]
      [(exp . args)
       (let ([slv (syntax-local-value #'exp (λ _ #f))])
         (and slv (expander? slv) (eh-mixin-expander? slv)))
       (let* ([slv (syntax-local-value #'exp (λ _ #f))]
              [transformer (expander-transformer slv)])
         (expand-no-order-clauses (transformer stx)))]
      [pat (parameterize ([current-no-order-clause #`#,(gensym 'clause)])
             (list (expand-all-eh-mixin-expanders #'pat)))]))

;; TODO: ~no-order should also be a eh-mixin-expander, so that nested ~post-fail
;; are caught 
(define-syntax ~no-order
  (pattern-expander
   (λ (stx)
     (syntax-case stx ()
       [(self pat ...)
        ((λ (x) (pretty-write (syntax->datum x)) (newline) x)
         (let ()
           (define acc '())
           (define (add-to-acc p)
             (set! acc (cons p #;(replace-context #'self p) acc)))
           (define alts
             (parameterize ([eh-post-accumulate add-to-acc])
               #;(expand-no-order-clauses #'(~or pat ...))
               (inline-or (expand-all-eh-mixin-expanders #'(~or pat ...)))))
           #`(~delimit-cut
              (~and (~seq (~or . #,alts) (... ...))
                    ~!
                    #,@acc))))]))))

(define-for-syntax (eh-post-accumulate! name p)
  (unless (eh-post-accumulate)
    (raise-syntax-error
     name
     (string-append (symbol->string name) " used outside of ~no-order")))
  ((eh-post-accumulate) p))

(define-eh-mixin-expander ~post-check
  (λ (stx)
    (syntax-case stx ()
      [(_ pat post)
       (begin
         (eh-post-accumulate! '~post-check #'post)
         #'pat)]
      [(_ post)
       (begin
         (eh-post-accumulate! '~post-check #'post)
         #'(~do))])))

(define-eh-mixin-expander ~post-fail
  (let ()
    (define (parse stx)
      (syntax-case stx ()
        [(_ message #:when condition)
         (begin
           #;(unless (current-no-order-clause)
               (raise-syntax-error
                '~post-fail
                "~post-fail cannot be used directly as an ellipsis-head pattern"))
           (define/with-syntax clause-present (gensym 'clause))
           (eh-post-accumulate!
            '~post-fail
            #`(~fail #:when (and (attribute (~bind [clause-present #t])
                                            #;#,(current-no-order-clause))
                                 condition)
                     message))
           #'(~do))]
        [(self #:when condition message)
         (parse #'(self message #:when condition))]))
    parse))

(define-syntax ~mutex
  (pattern-expander
   (λ (stx)
     (syntax-case stx ()
       [(self (mutex:id ...) pat ...)
        #'(???)]))))
