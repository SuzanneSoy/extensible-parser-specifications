#lang info
(define collection "extensible-parser-specifications")
(define deps '("base"
               "rackunit-lib"
               "phc-toolkit"
               "generic-syntax-expanders"
               "alexis-util"))
(define build-deps '("scribble-lib"
                     "racket-doc"
                     "seq-no-order"))
(define scribblings '(("scribblings/extensible-parser-specifications.scrbl" ())))
(define pkg-desc (string-append "Composable no-order syntax/parse"
                                " specifications, with global constraints"))
(define version "0.1")
(define pkg-authors '(|Suzanne Soy|))
