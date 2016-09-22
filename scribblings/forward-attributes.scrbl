#lang scribble/manual
@require[scribble/example
         "utils.rkt"
         @for-label[phc-toolkit/untyped
                    extensible-parser-specifications
                    generic-syntax-expanders
                    racket/base
                    syntax/parse
                    (only-in racket/base [... …])]]

@title{Chaining macro calls without re-parsing everything}

@defform[(define/syntax-parse+simple (name-or-curry . #,ntax-pattern) . body)
         #:grammar
         [(name-or-curry name
                         (name-or-curry arg ...))
          (maybe-define-class #:define-splicing-syntax-class class-id)
          (name identifier?)
          (class-id identifier?)]]{
 This macro works like @racket[define/syntax-parse] from @racket[phc-toolkit],
 except that it also defines the function @racket[_name-forward-attributes],
 which can be used by other macros to forward already parsed attributes to the
 @racket[body], without the need to parse everything a second time.

 The syntax pattern for the @racket[name] macro's arguments can be saved in a
 syntax class by specifying the @racket[#:define-splicing-syntax-class] option.

 If the caller macro which uses @racket[(_name-forward-attributes)] parsed its
 own @racket[stx] argument using @racket[class-id], then
 @racket[(_name-forward-attributes)] is equivalent to expanding
 @racket[(name stx)].

 The @racket[_name-forward-attributes] function is defined at the same meta
 level as @racket[name], i.e. at the same meta-level where this library was
 required. }


@defform[#:kind "for-template syntax"
         (define-syntax/parse+simple (name . #,ntax-pattern) . body)]{
 This macro is provided for meta-level -1.

 This is the same as @racket[define/syntax-parse+simple], except that it
 operates at level -1 relative to this library, and defines at that level a
 transformer binding (which therefore executes at the same meta-level as this
 library. In other words,
 @racket[(define-syntax/parse+simple (name . pat) . body)] is roughly equivalent
 to:

 @racketblock[
 (begin-for-syntax
   (define/syntax-parse+simple (tmp . pat) . body)
   (define name-forward-attributes tmp-forward-attributes))
 (define-syntax name tmp)]}
