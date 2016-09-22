#lang scribble/manual
@require[scribble/example
         "utils.rkt"
         @for-label[phc-toolkit/untyped
                    extensible-parser-specifications
                    generic-syntax-expanders
                    racket/base
                    syntax/parse
                    (only-in racket/base [... …])]]

@title{Parsing the tail of improper lists}

@defform[#:kind "eh-mixin expander"
         {~lift-rest pat}]{
                           
 Lifts @racket[pat] out of the current mixin, so that it is used as a pattern to
 match the tail of the improper list being matched. It is subject to the
 following restrictions:
 @itemlist[
 @item{@racket[~lift-rest] is allowed only within @racket[~no-order], but not
   within @racket[~seq-no-order]. @racket[~seq-no-order] always matches against
   a proper sequence of elements, while @racket[~no-order] may match a proper or
   improper list.}
 @item{The tail of the improper list must not be a pair, otherwise the
   @racket[car] would have been included in the main part of the list.}
 @item{The @racket[pat] is used to match the tail only if its surrounding
   pattern successfully matched some elements of the main section of the list.

   If the @racket[{~lift-rest pat}] is the only pattern present within an
   alternative, then it is always used.
   
   @examples[#:eval (make-evaluator)
             (syntax-parse #'(x y z . 1)
               [(~no-order {~lift-rest r:nat} i:id)
                (syntax->datum #'(r i ...))])]}
 @item{
   Among the lifted rest patterns which are considered (see the point
   above), only one may successfully match. An error is raised if two or more
   lifted rest patterns successfully match against the tail of the list.

   @examples[#:eval (make-evaluator)
             (eval:no-prompt
              (define p
                (syntax-parser
                  [(~no-order {~and {~literal x}
                                    {~lift-rest rn:nat}
                                    {~lift-rest ri:id}}
                              {~and {~literal y}
                                    {~lift-rest rs:str}
                                    {~lift-rest rj:id}})
                   'match]
                  #;[_
                   'fail])))
             (code:line (p #'(x . 1))   (code:comment "rn and ri considered, rn matched"))
             (code:line (p #'(x . z))   (code:comment "rn and ri considered, ri matched"))
             (code:line (p #'(y . "a")) (code:comment "rs and rj considered, rs matched"))
             (code:line (p #'(y . z))   (code:comment "rs and rj considered, rj matched"))
             (code:line (p #'(x y . 1)) (code:comment "all four considered, rn matched"))
             (eval:alts (code:line (p #'(x y . z)) (code:comment "all four considered, both ri and rj matched"))
                        (eval:error (p #'(x y . z))))]

   The rationale is that selecting the first lifted rest pattern that matches
   would result in unclear behaviour, as the order of the alternative clauses
   should not be significant.}
 @item{Post and global operations can be used within the @racket[pat]. This
   combination of features is not thoroughly tested, however. Please report any
   issues you run into.}]}
