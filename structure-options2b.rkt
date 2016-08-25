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
 ~post-fail
 ~nop)

;; ------------
;; eh-mixin — TODO: move to phc-toolkit once the PR #7 for reqprov in
;; generic-syntax-expander is merged.

(define-expander-type eh-mixin)

(define-for-syntax eh-post-accumulate (make-parameter #f))
(define-for-syntax clause-counter (make-parameter #f))

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

;; TODO: ~no-order should also be a eh-mixin-expander, so that when there are
;; nested ~no-order, the ~post-fail is caught by the nearest ~no-order.
(define-syntax ~no-order
  (pattern-expander
   (λ (stx)
     (syntax-case stx ()
       [(self pat ...)
        ((λ (x) (pretty-write (syntax->datum x)) (newline) x)
         (let ()
           (define acc '())
           (define counter 0)
           (define (increment-counter)
             (begin0 counter
                     (set! counter (add1 counter))))
           (define (add-to-acc p)
             (set! acc (cons p acc)))
           (define alts
             (parameterize ([eh-post-accumulate add-to-acc]
                            [clause-counter increment-counter])
               (inline-or (expand-all-eh-mixin-expanders #'(~or pat ...)))))
           #`(~delimit-cut
              (~and (~seq (~or . #,alts) (... ...))
                    ~!
                    #,@acc))))]))))

(define-syntax ~nop
  (pattern-expander
   (λ/syntax-case (_) () #'(~do))))

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
         #'(~nop))])))

(define-eh-mixin-expander ~post-fail
  (let ()
    (define (parse stx)
      (syntax-case stx ()
        [(_ message #:when condition)
         (begin
           (define/with-syntax clause-present
             (string->symbol (format "clause~a" ((clause-counter)))))
           (eh-post-accumulate!
            '~post-fail
            #`(~fail #:when (and (attribute clause-present)
                                 condition)
                     message))
           #'(~bind [clause-present #t]))]
        [(self #:when condition message)
         (parse #'(self message #:when condition))]))
    parse))

(define-syntax ~mutex
  (pattern-expander
   (λ (stx)
     (syntax-case stx ()
       [(self (mutex:id ...) pat ...)
        #'(???)]))))
