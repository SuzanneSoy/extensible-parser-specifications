#lang scribble/manual
@require[scribble/example
         "utils.rkt"
         @for-label[phc-toolkit/untyped
                    extensible-parser-specifications
                    generic-syntax-expanders
                    racket/base
                    syntax/parse
                    (only-in racket/base [... …])]]

@title{Matching alternatives in any order}

@defform[#:kind "pattern expander"
         #:literals (~mixin ~or)
         (~seq-no-order clause-or-mixin ...)
         #:grammar
         [(clause-or-mixin #,ntax-pattern
                           (~mixin #,-alternative-mixin)
                           (~or clause-or-mixin ...)
                           derived-or)]]{
 Splicing pattern which matches the given @racket[clause-or-mixin]s in any
 order, enforcing the global constraints expressed within each.

 Nested @racket[~or] directly below @racket[~seq-no-order] are recursively
 inlined. In other words, the @racket[~or] present directly below the
 @racket[~seq-no-order] or below such an @racket[~or] clause do not behave as
 "exclusive or", but instead contain clauses which can appear in any order.
 These clauses are not grouped in any way by the @racket[~or], i.e.
 @racket[(~no-order (~or (~or a b) (~or c d)))] is equivalent to
 @racket[(~no-order a b c d)].
                                         
 The @racket[derived-or] term covers any
 @tech[#:doc '(lib "syntax/scribblings/syntax.scrbl")]{pattern expander} or
 @tech{eh-mixin expander} application which expands to a
 @racket[clause-or-mixin]. The expansion of pattern and eh-mixin expanders
 happens before inlining the top @racket[~or] clauses.}

@defform[#:kind "pattern expander"
         #:literals (~mixin ~or)
         (~no-order clause-or-mixin ...)
         #:grammar
         [(clause-or-mixin #,ntax-pattern
                           (~mixin #,-alternative-mixin)
                           (~or clause-or-mixin ...)
                           derived-or)]]{
                                         
 Like @racket[~seq-no-order], except that it matches a syntax list, instead of
 being spliced into the surrounding sequence of patterns. In other words,

 @racketblock[({~seq-no-order clause-or-mixin ...})]
 
 is equivalent to (notice the extra pair of braces above):

 @racketblock[(~no-order clause-or-mixin ...)]

 Additionally, @racket[~no-order] can include clauses which use
 @racket[~lift-rest], which lifts a pattern which matches the tail of an
 improper list.}

@section{Enforcing a partial order on the alternatives}

@defform[#:kind "eh-mixin expander"
         (~order-point point-name #,ntax-pattern ...)]{
 When parsing a sequence of elements, @racket[~seq-no-order] and
 @racket[~no-order] associate an increasing number to each element starting from
 zero.
 
 The number associated with the first element matched by
 @racket[#,ntax-pattern ...] is memorised into the attribute
 @racket[point-name].

 This allows the position of elements matched by otherwise independent mixins to
 be compared using @racket[order-point<] and @racket[order-point>]}

@defform[(order-point< a b)
         #:grammar
         [(a #,tribute-name)
          (b #,tribute-name)]]{
 Returns @racket[#t] when the first element matched by
 @racket[(~order-point a #,ntax-pattern ...)] occurs before the first element
 matched by @racket[(~order-point b #,ntax-pattern ...)]. Otherwise, returns
 @racket[#f].

 This operation does not fail if @racket[a] or @racket[b] are bound to
 @racket[#f] (i.e. their corresponding @racket[_syntax-pattern ...] did not
 match). Instead, in both cases, it returns @racket[#f].}

@defform[(order-point> a b)
         #:grammar
         [(a #,tribute-name)
          (b #,tribute-name)]]{
 Returns @racket[#t] when the first element matched by
 @racket[(~order-point a #,ntax-pattern ...)] occurs after the first element
 matched by @racket[(~order-point b #,ntax-pattern ...)]. Otherwise, returns
 @racket[#f].

 This operation does not fail if @racket[a] or @racket[b] are bound to
 @racket[#f] (i.e. their corresponding @racket[_syntax-pattern ...] did not
 match). Instead, in both cases, it returns @racket[#f].}

@defform[(try-order-point< a b)
         #:grammar
         [(a #,tribute-name)
          (b #,tribute-name)]]{

 Like @racket[order-point<], except that it does not fail if @racket[a] or
 @racket[b] are not attributes, or if they are bound to @racket[#f]. Instead, in
 all those cases, it returns @racket[#f].

 It can be used as follows:

 @racketblock[
 (~post-fail "a must appear after b"
             #:when (try-order-point< a b))]

 The same caveats as for @racket[try-attribute] apply.}

@defform[(try-order-point> a b)
         #:grammar
         [(a #,tribute-name)
          (b #,tribute-name)]]{

 Like @racket[order-point>], except that it does not fail if @racket[a] or
 @racket[b] are not attributes, or if they are bound to @racket[#f]. Instead, in
 all those cases, it returns @racket[#f].

 It can be used as follows:

 @racketblock[
 (~post-fail "a must appear before b"
             #:when (try-order-point> a b))]

 The same caveats as for @racket[try-attribute] apply.}

@defform[#:kind "eh-mixin-expander"
         (~before other message pat ...)]{
                                              
 Post-checks that the first element matched by @racket[pat ...] appears before
 the @racket[other] order-point. This is a shorthand for:
 
 @racketblock[{~order-point pt
                {~seq pat ...}
                {~post-fail message #:when (order-point> pt other)}}]

 Note: Hopefully @racket[~before] will be modified in the future so that it
 auto-detects if the @racket[other] order-point is not defined as part of the
 current @racket[~no-order]. Do not rely on comparisons with order points
 somehow defined outside the current @racket[~no-order], as that behaviour may
 change in the future.

 This is implemented as a @seclink["Pre__global_and_post_operations"]{pre
  operation}.}

@defform[#:kind "eh-mixin-expander"
         (~after other message pat ...)]{
 Post-checks that the first element matched by @racket[pat ...] appears after
 the @racket[other] order-point. This is a shorthand for:
 
 @racketblock[{~order-point pt
                {~seq pat ...}
                {~post-fail message #:when (order-point< pt other)}}]

 Note: Hopefully @racket[~after] will be modified in the future so that it
 auto-detects if the @racket[other] order-point is not defined as part of the
 current @racket[~no-order]. Do not rely on comparisons with order points
 somehow defined outside the current @racket[~no-order], as that behaviour may
 change in the future.

 This is implemented as a @seclink["Pre__global_and_post_operations"]{pre
  operation}.}

@defform[#:kind "eh-mixin-expander"
         (~try-before other message pat ...)]{
                                              
 Post-checks that the first element matched by @racket[pat ...] appears before
 the @racket[other] order-point. The @racket[try-] version does not cause an
 error if the order-point @racket[other] is not define (e.g. it was part of
 another mixin which was not included). This is a shorthand for:
 
 @racketblock[{~order-point pt
                {~seq pat ...}
                {~post-fail message #:when (try-order-point> pt other)}}]

 Note: Hopefully @racket[~before] will be modified in the future so that it
 auto-detects if the @racket[other] order-point is missing. This form will then
 be removed.

 This is implemented as a @seclink["Pre__global_and_post_operations"]{pre
  operation}.}

@defform[#:kind "eh-mixin-expander"
         (~try-after other message pat ...)]{
 Post-checks that the first element matched by @racket[pat ...] appears after
 the @racket[other] order-point. The @racket[try-] version does not cause an
 error if the order-point @racket[other] is not define (e.g. it was part of
 another mixin which was not included). This is a shorthand for:
 
 @racketblock[{~order-point pt
                {~seq pat ...}
                {~post-fail message #:when (try-order-point< pt other)}}]

 Note: Hopefully @racket[~after] will be modified in the future so that it
 auto-detects if the @racket[other] order-point is missing. This form will then
 be removed.

 This is implemented as a @seclink["Pre__global_and_post_operations"]{pre
  operation}.}

