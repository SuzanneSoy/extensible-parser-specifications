#lang scribble/manual
@require[scribble/example
         "utils.rkt"
         @for-label[phc-toolkit/untyped
                    extensible-parser-specifications
                    generic-syntax-expanders
                    racket/base
                    syntax/parse
                    (only-in racket/base [... …])]]

@title{Miscellaneous pattern expanders}

@defform[#:kind "pattern expander"
         {~nop}]{
 The @tech[#:doc '(lib "syntax/scribblings/syntax.scrbl")
           #:key "action pattern"]{A-pattern} @racket[~nop] does not perform any
 action. It simply expands to @racket[{~do}].
}
