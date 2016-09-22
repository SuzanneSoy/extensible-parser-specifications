#lang scribble/manual
@require[scribble/example
         "utils.rkt"
         @for-label[phc-toolkit/untyped
                    extensible-parser-specifications
                    generic-syntax-expanders
                    racket/base
                    syntax/parse
                    (only-in racket/base [... …])]]

@title{Global operations}

The global patterns presented below match all of the given
@racket[#,ntax-pattern]s, like @racket[~and] does, and perform a global
aggregation over all the values corresponding to successful matches of a global
pattern using the same @racket[#,tribute-name].

After the whole @racket[~seq-no-order] or @racket[~no-order] finished matching
its contents, but before "post" operations are executed, the attribute
@racket[#,tribute-name] is bound to
@racket[(_aggrgate-function _value₁ ... _valueₙ)], where each @racket[valueᵢ] is
the value which was passed to an occurrence of @racket[~global-or] with the same
@racket[_attribute-name], and which successfully matched. The
@racket[_aggregate-function] will be @racket[or] for @racket[~global-or],
@racket[and] for @racket[~global-and] or @racket[+] for
@racket[~global-counter].

Each @racket[valueᵢ] is computed in the context in which it appears, after the
@racket[_syntax-pattern]s. This means that it can access:
@itemlist[
 @item{attributes already bound in the current alternative clause within the
  current @racket[~no-order] or @racket[~seq-no-order]}
 @item{attributes bound by the @racket[_syntax-patterns]s}
 @item{attributes already bound outside of the @racket[~no-order] or
  @racket[~seq-no-order]}
 @item{but it cannot access attributes bound in other alternative clauses within
  the current @racket[~no-order] or @racket[~seq-no-order].}]

The @racket[valueᵢ] are aggregated with @racket[or], @racket[and] or @racket[+]
in the order in which they appear in the @racket[~no-order] or
@racket[~seq-no-order]. If a @racket[valueᵢ] appears under ellipses, or as part
of an alternative clause which can match more than once (i.e. not @racket[~once]
or @racket[~optional]), then each match within that @racket[valueᵢ] group is
aggregated in the order it appears.

Since this notion of order is rather complex, it is possible that future
versions of this library will always return a boolean (@racket[#f] or
@racket[#t] for @racket[~global-or] and @racket[~global-and], which would make
the notion of order irrelevant.

@defform[#:kind "eh-mixin expander"
         (~global-or attribute-name+value #,ntax-pattern ...)
         #:grammar
         [(attribute-name+value #,tribute-name
                                [#,tribute-name valueᵢ])]]{
 Matches all of the given @racket[#,ntax-pattern]s, like @racket[~and] does, and
 perform a global @racket[or] over all the values corresponding to successful
 matches of a global pattern using the same @racket[#,tribute-name]. See above
 for a description of how global operations work.
                                                     
 If the @racket[valueᵢ] is omitted, @racket[#t] is used as a default.

 The result is always transformed into a boolean, so @racket[_attribute-name] is
 always bound to either @racket[#t] or @racket[#f].}

@defform[#:kind "eh-mixin expander"
         (~global-and attribute-name+value #,ntax-pattern ...)
         #:grammar
         [(attribute-name+value [#,tribute-name valueᵢ])]]{
 Matches all of the given @racket[#,ntax-pattern]s, like @racket[~and] does, and
 perform a global @racket[and] over all the values corresponding to successful
 matches of a global pattern using the same @racket[#,tribute-name]. See above
 for a description of how global operations work.

 If there is at least one occurrence of @racket[~global-and] for that
 @racket[_attribute-name] which successfully matches, the result of the
 @racket[(and valueᵢ ...)] is always coerced to a boolean, so
 @racket[_attribute-name] is always bound to either @racket[#t] or @racket[#f].

 If there are no matches at all, the special value @racket['none] is used
 instead of @racket[#t] as would be produced by @racket[(and)].}

@defform[#:kind "eh-mixin expander"
         (~global-counter attribute-name+value #,ntax-pattern ...)
         #:grammar
         [(attribute-name+value #,tribute-name
                                [#,tribute-name valueᵢ])]]{
 Matches all of the given @racket[#,ntax-pattern]s, like @racket[~and] does, and
 perform a global @racket[+] over all the values corresponding to successful
 matches of a global pattern using the same @racket[#,tribute-name]. See above
 for a description of how global operations work.

 If the @racket[valueᵢ] is omitted, @racket[1] is used as a default.}

@;@defform[(aggregate-global-or)]
@;@defform[(aggregate-global-and)]
@;@defform[(aggregate-global-counter)]
