#lang racket/base

(provide define-syntax/parse+simple
         (for-syntax define/syntax-parse+simple))

(require phc-toolkit/untyped
         syntax/parse/define
         (for-syntax racket/base
                     syntax/parse
                     racket/stxparam
                     racket/syntax)
         (for-meta 2
                   racket/base
                   syntax/parse
                   racket/syntax
                   phc-toolkit/untyped
                   (prefix-in syntax/parse: syntax/parse/private/residual-ct)))

(define-simple-macro (define-syntax/parse+simple [name stxclass] . body)
  #:with name-forward (format-id #'name "~a-forward-attributes" #'name)
  #:with tmp-forward (format-id #'tmp "~a-forward-attributes" #'tmp)
  (begin
    (begin-for-syntax
      (define/syntax-parse+simple [tmp stxclass] . body)
      (define-syntax name-forward (make-rename-transformer #'tmp-forward)))
    (define-syntax name tmp)))

(begin-for-syntax
  (define-syntax define/syntax-parse+simple
    (syntax-parser
      [(_ [name (~var cls (static syntax/parse:stxclass? "a syntax class"))]
          . body)
       #:with colon-stxclass (format-id #'cls ":~a" #'cls)
       #:with name-forward (format-id #'name "~a-forward-attributes" #'name)
       (with-disappeared-uses
        (define c (syntax-local-value/record #'cls syntax/parse:stxclass?))
        (define attrs (syntax/parse:stxclass-attrs c))
        (define/with-syntax (attr-name …) (map syntax/parse:attr-name attrs))
        (define/with-syntax (attr-name/ctx …)
          (stx-map (λ (a) (datum->syntax #'body (syntax-e a)))
                   #'(attr-name …)))
        (define-temp-ids "~a/arg" (attr-name …))
        (define/with-syntax (attr-depth …) (map syntax/parse:attr-depth attrs))
        #'(begin
            (define (name stx2)
              (syntax-parameterize ([stx (make-rename-transformer #'stx2)])
                (syntax-parse stx2
                  [(name colon-stxclass) . body])))
            (define (private-simple-api stx/arg attr-name/arg …)
              (syntax-parameterize ([stx (make-rename-transformer #'stx/arg)])
                (syntax-parse #'nothing
                  [(~bind [(attr-name/ctx attr-depth) attr-name/arg] …)
                   . body])))
            (define-syntax (name-forward stx3)
              (syntax-case stx3 ()
                [(_)
                 #`(private-simple-api
                    stx
                    (attribute #,(datum->syntax stx3 'attr-name))
                    …)]
                [(_ forward-args-prefix)
                 #`(private-simple-api
                    stx
                    (attribute #,(format-id stx3 "~a.~a"
                                            #'forward-args-prefix
                                            'attr-name))
                    …)]))))])))