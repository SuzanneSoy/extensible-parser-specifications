#lang scribble/manual
@require[scribble/example
         "utils.rkt"
         @for-label[phc-toolkit/untyped
                    extensible-parser-specifications
                    generic-syntax-expanders
                    racket/base
                    syntax/parse
                    (only-in racket/base [... …])]]

@title{extensible-parser-specifications}
@author{@author+email["Suzanne Soy" "racket@suzanne.soy"]}

Caveat: the mixins defined with @racket[define-eh-alternative-mixin] cannot be
provided and used in a separate module. Unfortunately, I cannot think of an
acceptable fix for this problem, as solving this would require extracting parts
of the mixin while preserving the bindings of some identifiers, but altering the
bindings of others. This means that for the foreseeable future, once a mixin is
defined, can only be used via @racket[~mixin] (or by directly invoking it)
within the same module.

The regular and splicing syntax classes defined with
@racket[#:define-syntax-class] and @racket[#:define-splicing-syntax-class] will
work fine across module boundaries, however. Manually defined syntax classes,
splicing syntax classes or ellipsis-head syntax classes will also work fine
across module boundaries, even if they contain uses of @racket[~no-order] and
@racket[~seq-no-order], and even if those special forms contain uses of mixins
defined within the same module. In other words, as long as a definition of a
mixin and all its uses via @racket[~mixin] are within the same module,
everything else should work without surprises.

@defmodule[extensible-parser-specifications]

@include-section{defining-reusable-mixins.scrbl}
@include-section{no-order.scrbl}
@include-section{rest.scrbl}

@include-section{pre-global-post-section.scrbl}

@include-section{misc.scrbl}
@include-section{forward-attributes.scrbl}

