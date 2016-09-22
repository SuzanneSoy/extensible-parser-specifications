#lang scribble/manual
@require[scribble/example
         "utils.rkt"
         @for-label[phc-toolkit/untyped
                    extensible-parser-specifications
                    generic-syntax-expanders
                    racket/base
                    syntax/parse
                    (only-in racket/base [... …])]]

@title{Pre operations}

@defform[#:kind "eh-mixin expander"
         (~named-seq #,tribute-name #,ntax-pattern ...)]{
 Equivalent to @racket[{~seq #,ntax-pattern ...}], but also binds the
 @racket[#,tribute-name] to the whole sequence. If the sequence appears inside
 an @racket[~optional] or @racket[~or] clause that fails, the
 @racket[_attribute-name] is still bound to the empty sequence.

 Known issues: this may not behave as expected if @racket[~named-seq] appears
 under ellipses.
 
 This probably should bind the sequence attribute @emph{before} the "global"
 operations, instead of being a "post" operation, and may be changed in that way
 the future.}

@defform[#:kind "eh-mixin expander"
         (~maybe/empty #,ntax-pattern ...)]{

 Optionally matches @racket[{~seq #,ntax-pattern ...}]. If the match fails, it
 matches these same sequence of patterns against the empty syntax list
 @racket[#'()]. This form can be used in an ellipsis-head position. This is
 implemented in both cases as a "pre" action.}

@defform[#:kind "eh-mixin expander"
         (~optional/else #,ntax-pattern
                         maybe-defaults
                         else-post-fail ...
                         maybe-name)
         #:grammar
         [(maybe-defaults (code:line)
                          (code:line #:defaults (default-binding ...)))
          (else-post-fail
           (code:line #:else-post-fail message #:when condition)
           (code:line #:else-post-fail #:when condition message)
           (code:line #:else-post-fail message #:unless unless-condition)
           (code:line #:else-post-fail #:unless unless-condition message))
          (maybe-name (code:line)
                      (code:line #:name #,tribute-name))]]{
 Like @racket[~optional], but with conditional post-failures when the pattern is
 not matched. An @racket[~optional/else] pattern can be matched zero or one time
 as part of the @racket[~seq-no-order] or @racket[~no-order]. When it is not
 matched (i.e. matched zero times):
 @itemlist[
 @item{it uses the default values for the attributes as specified with
   @racket[#:defaults].}
 @item{for each @racket[#:else-post-fail] clause, it checks whether the
   @racket[condition] or @racket[unless-condition] is true or false,
   respectively. If this is the case the whole @racket[~seq-no-order] or
   @racket[~no-order] is rejected with the given @racket[_message]. The
   behaviour of @racket[#:else-post-fail] is the same as the behaviour of
   @racket[~post-fail], except that the "post" conditional failure can only be
   executed if the optional @racket[_syntax-pattern] was not matched.

   Note that there is an implicit cut (@racket[~!]) between the no-order
   patterns and the "post" checks, so after a @racket[~post-fail] fails,
   @racket[syntax-parse] does not backtrack and attempt different combinations
   of patterns to match the sequence, nor does it backtrack and attempt to match
   a shorter sequence. This is by design, as it allows for better error messages
   (syntax-parse would otherwise attempt and possibly succeed in matching a
   shorter sequence, then just treat the remaining terms as
   "unexpected terms").}]

 The meaning of @racket[#:name #,tribute-name] option is the same as for
 @racket[~optional].}
