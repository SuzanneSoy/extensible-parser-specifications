#lang info
(define collection "extensible-parser-specifications")
(define deps '("base"
               "rackunit-lib"
               "https://github.com/jsmaniac/phc-toolkit.git"
               "generic-syntax-expanders"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/extensible-parser-specifications.scrbl" ())))
(define pkg-desc "Description Here")
(define version "0.1")
(define pkg-authors '(|Georges Dupéron|))
