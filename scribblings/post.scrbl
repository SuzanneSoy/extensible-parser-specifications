#lang scribble/manual
@require[scribble/example
         "utils.rkt"
         @for-label[phc-toolkit/untyped
                    extensible-parser-specifications
                    generic-syntax-expanders
                    racket/base
                    syntax/parse
                    (only-in racket/base [... …])]]

@title{Post operations}

@defform*[#:kind "eh-mixin expander"
          [(~post-check #,ntax-pattern #,A-patte)
           (~post-check #,A-patte)]]{
 Matches @racket[#,ntax-pattern], and executes the given @racket[#,A-patte]
 after the whole @racket[~seq-no-order] or @racket[~no-order] finished matching
 its contents.
 
 If unspecified, the @racket[_syntax-pattern] defaults to @racket[(~nop)].}

@defform*[#:kind "eh-mixin expander"
          [(~post-fail message #:when condition)
           (~post-fail #:when condition message)
           (~post-fail message #:unless unless-condition)
           (~post-fail #:unless unless-condition message)]]{
                                                   
 After the whole @racket[~seq-no-order] or @racket[~no-order] finished matching
 its contents, checks whether @racket[condition] or @racket[unless-condition] is
 true or false, respectively.  If this is the case the whole
 @racket[~seq-no-order] or @racket[~no-order] is rejected with the given
 @racket[_message].

 Note that there is an implicit cut (@racket[~!]) between the no-order patterns
 and the "post" checks, so after a @racket[~post-fail] fails,
 @racket[syntax-parse] does not backtrack and attempt different combinations of
 patterns to match the sequence, nor does it backtrack and attempt to match a
 shorter sequence. This is by design, as it allows for better error messages
 (syntax-parse would otherwise attempt and possibly succeed in matching a
 shorter sequence, then just treat the remaining terms as "unexpected terms").}
