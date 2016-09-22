#lang scribble/manual
@require[scribble/example
         "utils.rkt"
         @for-label[phc-toolkit/untyped
                    extensible-parser-specifications
                    generic-syntax-expanders
                    racket/base
                    syntax/parse
                    (only-in racket/base [... …])]]

@title{Order in which the attributes are bound for post operations and
 global operations}

Within the @racket[_A-pattern]s of post operations, the regular attributes bound
by all the clauses inside @racket[~seq-no-order] or @racket[~no-order] are
bound. The attributes defined as part of all "global" actions are bound too. The
attributes defined as part of "post" actions of other clauses are bound only if
the clause defining them appears before the current clause in the source code.
For example, the following code works because the clause containing
@racket[{~post-fail "2 is incompatible with 1" #:when (not (attribute a))}]
appears after the clause which binds @racket[a] with the "post" action
@racket[{~post-check {~bind ([a #'the-a])}}].

@racketblock[
 {~seq-no-order
  {~post-check {~and the-a 1} {~bind ([a #'the-a])}}
  {~and 2 {~post-fail "2 is incompatible with 1" #:when (not (attribute a))}}}]

If the two clauses are swapped, then the following code would raise a syntax
error because @racket[a] is not bound as an attribute in the
@racket[~post-fail]:

@racketblock[
 {~seq-no-order
  {~and 2 {~post-fail "2 is incompatible with 1" #:when (not (attribute a))}}
  {~post-check {~and the-a 1} {~bind ([a #'the-a])}}}]

On the other hand, the following code, which does not bind @racket[a] as part
of a post operation, is valid:
 
@racketblock[
 {~seq-no-order
  {~and 2 {~post-fail "2 is incompatible with 1" #:when (not (attribute a))}}
  {~and the-a 1 {~bind ([a #'the-a])}}}]

Furthermore, the following code still works, as attributes are bound by the
"global" operations before the "post" operations are executed:

@racketblock[
 {~seq-no-order
  {~and 2 {~post-fail "2 is incompatible with 1" #:when (not (attribute a))}}
  {~global-or a 1}}]

Note that the order in which clauses appear within the @racket[~seq-no-order]
or @racket[~no-order] does not impact the order in which the elements must
appear in the matched syntax (aside from issues related to greediness).

@defform[(try-attribute #,tribute-name)]{
 This macro expands to @racket[(attribute #,tribute-name)] if
 @racket[#,tribute-name] is bound as a syntax pattern variable, and to
 @racket[#f] otherwise.

 This macro can be used to check for mutual exclusion of an attribute which is
 bound by other mixins that might or might not be present in the final
 @racket[~no-order] or @racket[~seq-no-order].
 
 Use this sparingly, as if an syntax pattern variable with that name is bound by
 an outer scope, the @racket[try-attribute] macro will still access it, ignorant
 of the fact that the current @racket[~seq-no-order] does not contain any mixin
 which binds that attribute.

 Instead, it is better practice to use
 @racket[{~global-or [_attribute-name #f]}] or
 @racket[{~global-and [_attribute-name #t]}] to ensure that the attribute is
 declared, while using the operation's neutral element to not alter the final
 result.}

@defform[(if-attribute #,tribute-name if-branch else-branch)]{
 This macro expands to @racket[if-branch] if @racket[#,tribute-name] is bound as
 a syntax pattern variable, and to @racket[else-branch] otherwise.

 The same caveats as for @racket[try-attribute] apply.}