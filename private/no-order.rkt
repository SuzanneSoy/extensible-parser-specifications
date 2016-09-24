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
         racket/list
         racket/function
         racket/format
         (for-syntax racket/base
                     syntax/parse
                     racket/syntax
                     phc-toolkit/untyped
                     racket/list
                     racket/pretty)
         "parameters.rkt"
         "try-attribute.rkt")

(provide define-eh-alternative-mixin
         ~seq-no-order
         ~no-order
         ~order-point
         order-point<
         order-point>
         try-order-point<
         try-order-point>
         ~lift-rest
         ~omitable-lifted-rest ;; Private
         (expander-out eh-mixin)) ;; Private

(define-expander-type eh-mixin)

(define-syntax define-eh-alternative-mixin
  (syntax-parser
    [(_ name
        (~maybe #:define-splicing-syntax-class splicing-name)
        (~maybe #:define-syntax-class class-name)
        ((~literal pattern) pat) ...)
     #`(begin
         (define-eh-mixin-expander name
           (λ (_)
             (syntax-local-syntax-parse-pattern-introduce
              (quote-syntax (~or pat ...)))))
         #,@(if (attribute splicing-name)
                #'((define-splicing-syntax-class splicing-name
                     (pattern {~seq-no-order {name}})))
                #'())
         #,@(if (attribute class-name)
                #'((define-syntax-class class-name
                     (pattern {~no-order {name}})))
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

;; TODO: this does not work when there is a pattern expander which expands to
;; an ~or^eh
(define-for-syntax (catch-omitable-lifted-rest stx)
  (define caught '())
  (define (r stx)
    ;(displayln (list r stx))
    (cond
      [(syntax? stx) (datum->syntax stx (r (syntax-e stx)) stx stx)]
      [(and (pair? stx)
            (identifier? (car stx))
            (free-identifier=? (car stx) #'~or))
       (cons (car stx) (l (cdr stx)))]
      [(and (pair? stx)
            (identifier? (car stx))
            (free-identifier=? (car stx) #'~omitable-lifted-rest))
       (set! caught (cons stx caught))
       #'{~or}] ;; empty ~or with no eh alternatives
      [else stx]))
  (define (l stx)
    ;(displayln (list l stx))
    (cond
      [(syntax? stx) (datum->syntax stx (r (syntax-e stx)) stx stx)]
      [(list? stx) (map r stx)]
      [(pair? stx) (cons (r (car stx)) (l (cdr stx)))]
      [else stx]))
  (define cleaned (r stx))
  (values cleaned caught))

;; TODO: ~seq-no-order should also be a eh-mixin-expander, so that when there
;; are nested ~seq-no-order, the ~post-fail is caught by the nearest
;; ~seq-no-order.


(define-for-syntax ((no-order-ish seq?) stx)
  (syntax-case stx ()
    [(self pat ...)
     (with-disappeared-uses*
      (define counter 0)
      (define (increment-counter!)
        (begin0 counter
                (set! counter (add1 counter))))
      ;; first, pre and post-acc gather a-patterns which will be added after
      ;; the (~seq (~or ) ...), before and after the ~! cut respectively
      (define first-acc '())
      (define (add-to-first! v) (set! first-acc (cons v first-acc)))
      (define pre-acc '())
      (define (add-to-pre! v) (set! pre-acc (cons v pre-acc)))
      (define post-acc '())
      (define (add-to-post! v) (set! post-acc (cons v post-acc)))
      ;; post-groups-acc gathers some attributes that have to be grouped
      (define post-groups-acc '())
      (define (add-to-post-groups! . v)
        (set! post-groups-acc (cons v post-groups-acc)))
      (define lifted-rest '())
      (define (add-to-lift-rest! present-clause expanded-pat)
        (define succeeded (get-new-clause!))
        (set! lifted-rest (cons (list present-clause
                                      expanded-pat
                                      succeeded)
                                lifted-rest)))
      ;; expand EH alternatives:
      (parameterize ([eh-first-accumulate add-to-first!]
                     [eh-pre-accumulate add-to-pre!]
                     [eh-post-group add-to-post-groups!]
                     [eh-post-accumulate add-to-post!]
                     [clause-counter increment-counter!]
                     [lift-rest add-to-lift-rest!])
        (define alts
          (expand-all-eh-mixin-expanders #'(~or pat ...)))
        ;; TODO: we can probably close the "parameterize" here.



        
        ;; NOTE: this works only because eh-mixin-expanders are NOT pattern
        ;; expanders. If these are merged later on, then this needs to be
        ;; adjusted
        (define-values (cleaned-alts caught-omitable-lifted-rest)
          (catch-omitable-lifted-rest alts))
        (define post-group-bindings
          (for/list ([group (group-by car
                                      (reverse post-groups-acc)
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
        (set! lifted-rest (reverse lifted-rest))
        (define/with-syntax whole-clause (get-new-clause!))
        (define/with-syntax rest-clause (get-new-clause!))
        (define/with-syntax parse-seq-order-sym-id
          (datum->syntax (parse-seq-order-sym-introducer
                          (syntax-local-introduce #'here))
                         'parse-seq-order-sym))
        (define/with-syntax whole-clause-pat
          (if seq?
              (begin
                (when (not (null? lifted-rest))
                  (raise-syntax-error
                   '~seq-no-order
                   (string-append "rest clause must be used within ~no-order,"
                                  " but was used within ~seq-no-order")
                   stx))
                #'{~seq whole-clause (… …) {~bind [(rest-clause 1) (list)]}})
              #'(whole-clause (… …) . {~and rest-clause {~not (_ . _)}})))
        (define rest-handlers
          (if (null? lifted-rest)
              #'()
              (with-syntax ([[(present expanded-pat succeeded) …] lifted-rest])
                #'({~parse
                    {~or (_ {~parse #t
                                    (ormap identity
                                           (flatten (attribute present)))}
                            {~parse expanded-pat
                                    #'rest-clause}
                            {~bind [succeeded #t]})
                         …
                         (_ {~fail (~a "expected one of the rest patterns"
                                       " to match")})}
                    #'(dummy)}))))
        (define check-no-dup-rest-handlers
          (if (null? lifted-rest)
              #'()
              (with-syntax ([([present expanded-pat succeeded] …) lifted-rest])
                #'({~fail #:when (or (and (not (attribute succeeded))
                                          (ormap identity
                                                 (flatten (attribute present)))
                                          (syntax-parse #'rest-clause
                                            [expanded-pat #t]
                                            [_ #f]))
                                     …)
                          (~a "more than one of the lifted rest patterns"
                              " matched")}))))

        ((λ (x) #;(pretty-write (syntax->datum #`(syntax-parser [#,x 'ok]))) x)
         #`(~delimit-cut
            (~and #,(fix-disappeared-uses)
                  whole-clause-pat
                  {~do (define parse-seq-order-sym-id
                         (gensym 'parse-seq-order))}
                  {~parse ({~seq #,cleaned-alts (… …)})
                          #`#,(for/list
                                  ([xi (in-syntax #'(whole-clause (… …)))]
                                   [i (in-naturals)])
                                ;; Add a syntax property before parsing,
                                ;; to track the position of matched elements
                                ;; using ~order-point
                                (syntax-property xi
                                                 parse-seq-order-sym-id
                                                 i))}
                  #,@(reverse first-acc)
                  #,@(reverse pre-acc)
                  #,@caught-omitable-lifted-rest
                  #,@rest-handlers
                  ~!
                  #,@check-no-dup-rest-handlers
                  (~bind #,@post-group-bindings)
                  #,@(reverse post-acc))))))]))

(define-syntax ~seq-no-order (pattern-expander (no-order-ish #t)))
(define-syntax ~no-order (pattern-expander (no-order-ish #f)))

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

(define-syntax-rule (try-order-point< a b)
  (if-attribute a (if-attribute b (order-point< a b) #f) #f))

(define-syntax-rule (try-order-point> a b)
  (if-attribute a (if-attribute b (order-point> a b) #f) #f))

(define-syntax ~omitable-lifted-rest
  (pattern-expander
   (λ (stx)
     (syntax-case stx ()
       [(_ expanded-pats clause-present)
        #'{~and
           ;; TODO: copy the disappeared uses instead of this hack
           {~do 'expanded-pats}
           {~bind [clause-present #t]}}]))))

(define-eh-mixin-expander ~lift-rest
  (λ (stx)
    (syntax-case stx ()
      [(_ pat)
       (let ()
         (define/with-syntax clause-present (get-new-clause!))
         (define/with-syntax expanded-pat
           ;; let the ~post, ~global etc. within pat … be recognized
           (expand-all-eh-mixin-expanders #'pat))
         (lift-rest! '~lift-rest #'clause-present #'expanded-pat)
         #'(~omitable-lifted-rest expanded-pat clause-present))])))