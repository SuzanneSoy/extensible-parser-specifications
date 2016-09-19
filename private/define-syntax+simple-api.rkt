#lang racket/base

(provide define-syntax/parse+simple
         (for-syntax define/syntax-parse+simple))

(require phc-toolkit/untyped
         syntax/parse/define
         (for-syntax racket/base
                     syntax/parse
                     racket/stxparam
                     racket/syntax
                     phc-toolkit/untyped
                     "parameters.rkt")
         (for-meta 2 (prefix-in syntax/parse: syntax/parse/private/residual-ct))
         (for-meta 2 racket/base)
         (for-meta 2 racket/list)
         (for-meta 2 racket/syntax)
         (for-meta 2 syntax/parse)
         (for-meta 2 phc-toolkit/untyped))

(define-syntax/parse (define-syntax/parse+simple
                       [name . args] . body)
  (with-format-ids/inject-binders
   ([name-forward #'name "~a-forward-attributes" #'name]
    [tmp-forward  #'tmp  "~a-forward-attributes" #'tmp])
   #'(begin
       (begin-for-syntax
         (inject-sub-range-binders ...
          (define/syntax-parse+simple [tmp . args] . body)
          (define-syntax name-forward (make-rename-transformer #'tmp-forward))))
       (define-syntax name tmp))))

(begin-for-syntax
  (define-syntax (define/syntax-parse+simple stx)
    (syntax-parse stx
      [(_ (name:name-or-curry . args)
          (~optional (~seq #:define-splicing-syntax-class define-class-name:id))
          . body)
       (let ()
         (define introducer (make-syntax-introducer))
         (define/with-syntax args-stxclass
           (or (attribute define-class-name)
               (introducer (datum->syntax #'args 'args-stxclass) 'add)))
         (define/with-syntax body-introduced
           (introducer #'body 'add))
         #'(begin
             (define-splicing-syntax-class args-stxclass
               #:auto-nested-attributes
               (pattern (~seq . args)))
             (define/syntax-parse+simple/stxclass [name args-stxclass]
               . body-introduced)))]))

  (define-for-syntax (change-name-or-curry stx new-name)
    (if (identifier? stx)
        new-name
        #`(#,(change-name-or-curry (stx-car stx) new-name) . #,(stx-cdr stx))))

  #;(define-for-syntax (pat-name-or-curry stx new-name)
      (if (identifier? stx)
          new-name
          #`(#,(pat-name-or-curry (stx-car stx) new-name) . #,(gensym 'args))))
  
  (define-syntax define/syntax-parse+simple/stxclass
    (syntax-parser-with-arrows
     [(_ [name:name-or-curry
          (~var cls (static syntax/parse:stxclass? "a syntax class"))]
         . body)
      #:with colon-stxclass (format-id #'cls ":~a" #'cls)
      #:with name-forward (format-id/record #'name.id
                                            "~a-forward-attributes" #'name.id)
      (define c (syntax-local-value/record #'cls syntax/parse:stxclass?))
      (define attrs (filter-not (λ (a) (is-clause-id-sym?
                                        (syntax/parse:attr-name a)))
                                (syntax/parse:stxclass-attrs c)))
      (define/with-syntax (attr-name …) (map syntax/parse:attr-name attrs))
      (define/with-syntax (attr-name/ctx …)
        (stx-map (λ (a) (datum->syntax #'body (syntax-e a)))
                 #'(attr-name …)))
      (define-temp-ids "~a/arg" (attr-name …))
      (define/with-syntax (attr-depth …)
        (map syntax/parse:attr-depth attrs))
      (define/with-syntax def-private-simple-api
        (change-name-or-curry #'name
                              #'(private-simple-api stx/arg attr-name/arg …)))
      (syntax/top-loc this-syntax
        (begin
          (define (name stx2)
            (with-arrows
             (syntax-parameterize ([stx (make-rename-transformer #'stx2)])
               (syntax-parse stx2
                 [(_ colon-stxclass) . body]))))
          (define def-private-simple-api
            (syntax-parameterize ([stx (make-rename-transformer #'stx/arg)])
              (syntax-parse #'nothing
                [(~bind [(attr-name/ctx attr-depth) attr-name/arg] …)
                 . body])))
          (define-syntax (name-forward stx3)
            (syntax-case stx3 ()
              [(_)
               (quasisyntax/top-loc stx3
                 (private-simple-api
                  stx
                  (attribute #,(datum->syntax stx3 'attr-name))
                  …))]
              [(_ forward-args-prefix)
               (identifier? #'forward-args-prefix)
               (quasisyntax/top-loc stx3
                 (private-simple-api
                  stx
                  (attribute #,(format-id stx3 "~a.~a"
                                          #'forward-args-prefix
                                          'attr-name))
                  …))]))))])))