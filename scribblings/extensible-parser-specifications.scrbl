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
@author{@author+email["Georges Dupéron" "georges.duperon@gmail.com"]}

@defmodule[extensible-parser-specifications]

@include-section{defining-reusable-mixins.scrbl}
@include-section{no-order.scrbl}
@include-section{rest.scrbl}

@include-section{pre-global-post-section.scrbl}

@include-section{misc.scrbl}
@include-section{forward-attributes.scrbl}

