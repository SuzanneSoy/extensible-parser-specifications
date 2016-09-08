#lang racket/base

;; TODO: it should be possible to specify a partial ordering and/or join the
;; constraints for multiple occurrences of the same ~no-order clause. The goal
;; is to be able to write a pattern for:
;; (some-macro opts … name opts … field … opts …)
;; where opts is a ~no-order, with some constraints like ~once, ~optional etc.
;; I'd like to write something like:
;; (some-macro (~no-order #:kw1
;;                        (~seq #:kw2 opt2)
;;                        name:id
;;                        (~and fields
;;                              (~seq field:id …)
;;                              (~global-before name fields))))
;; However, the current implementation uses "(~or no-order-clauses) …" which
;; does not permit a clause to see previously matched clauses.
;; Maybe save this for the unified parser and generator library (see on github
;; the repo jsmaniac/phc-toolkit, more specifically the file
;; scribblings/template.scrbl within).

(require syntax/parse
         ;syntax/parse/experimental/eh
         generic-syntax-expanders
         phc-toolkit/untyped
         (for-syntax racket/base
                     syntax/parse
                     racket/syntax
                     phc-toolkit/untyped
                     racket/list
                     racket/pretty)
         "parameters.rkt")

(provide define-eh-alternative-mixin
         ~seq-no-order
         ~no-order
         ~order-point
         order-point<
         order-point>
         (expander-out eh-mixin))

(define-expander-type eh-mixin)

(define-syntax define-eh-alternative-mixin
  (syntax-parser
    [(_ name (~maybe #:define-splicing-syntax-class splicing-name)
        ((~literal pattern) pat) ...)
     #`(begin
         (define-eh-mixin-expander name
           (λ (_)
             (syntax-local-syntax-parse-pattern-introduce
              (quote-syntax (~or pat ...)))))
         #,@(if (attribute splicing-name)
                #'((define-splicing-syntax-class splicing-name
                     (pattern {~seq-no-order {name}})))
                #'()))]))

(define-for-syntax (inline-or stx)
  (syntax-case stx ()
    [(o . rest)
     (and (identifier? #'o) (free-identifier=? #'o #'~or))
     (apply append (stx-map inline-or #'rest))]
    [x (list #'x)]))

(define-for-syntax parse-seq-order-sym-introducer (make-syntax-introducer))

(define-for-syntax (fix-disappeared-uses)
  ;; Fix for https://github.com/racket/racket/issues/1452
  (let ([dis (current-recorded-disappeared-uses)])
    #`{~do #,(with-disappeared-uses*
              (record-disappeared-uses dis)
              #'(void))}))

;; TODO: ~seq-no-order should also be a eh-mixin-expander, so that when there
;; are nested ~seq-no-order, the ~post-fail is caught by the nearest
;; ~seq-no-order.
(define-syntax ~seq-no-order
  (pattern-expander
   (λ (stx)
     (syntax-case stx ()
       [(self pat ...)
        (with-disappeared-uses*
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
         (parameterize ([eh-post-accumulate add-to-post!]
                        [eh-post-group add-to-post-groups!]
                        [clause-counter increment-counter])
           (define alts
             (expand-all-eh-mixin-expanders #'(~or pat ...)))
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
           (define/with-syntax whole-clause (get-new-clause!))
           (define/with-syntax parse-seq-order-sym-id
             (datum->syntax (parse-seq-order-sym-introducer
                             (syntax-local-introduce #'here))
                            'parse-seq-order-sym))
           #`(~delimit-cut
              (~and #,(fix-disappeared-uses)
                    {~seq whole-clause (… …)}
                    {~do (define parse-seq-order-sym-id
                           (gensym 'parse-seq-order))}
                    {~parse ({~seq #,alts (… …)})
                            #`#,(for/list
                                    ([xi (in-syntax #'(whole-clause (… …)))]
                                     [i (in-naturals)])
                                  ;; Add a syntax property before parsing,
                                  ;; to track the position of matched elements
                                  ;; using ~order-point
                                  (syntax-property xi
                                                   parse-seq-order-sym-id
                                                   i))}
                    ~!
                    (~bind #,@post-group-bindings)
                    #,@post-acc))))]))))

(define-syntax ~no-order
  (pattern-expander
   (λ/syntax-case (_ . rest) ()
     #'({~seq-no-order . rest}))))

(define-eh-mixin-expander ~order-point
  (λ (stx)
    (define/with-syntax clause-point (get-new-clause!))
    (define/with-syntax parse-seq-order-sym-id
      (datum->syntax (parse-seq-order-sym-introducer
                      (syntax-local-introduce #'here))
                     'parse-seq-order-sym))
    (syntax-case stx ()
      [(_ point-name pat …)
       #'(~and (~seq clause-point _ (… …))
               (~bind [point-name (syntax-property #'clause-point
                                                   parse-seq-order-sym-id)])
               {~seq pat …})])))

(define-syntax-rule (order-point< a b)
  (and (attribute a) (attribute b)
       (< (attribute a) (attribute b))))

(define-syntax-rule (order-point> a b)
  (and (attribute a) (attribute b)
       (> (attribute a) (attribute b))))