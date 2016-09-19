[![Build Status,](https://img.shields.io/travis/jsmaniac/extensible-parser-specifications/master.svg)](https://travis-ci.org/jsmaniac/extensible-parser-specifications)
[![Coverage Status,](https://img.shields.io/coveralls/jsmaniac/extensible-parser-specifications/master.svg)](https://coveralls.io/github/jsmaniac/extensible-parser-specifications)
[![Build Stats,](https://img.shields.io/badge/build-stats-blue.svg)](http://jsmaniac.github.io/travis-stats/#jsmaniac/extensible-parser-specifications)
[![Online Documentation.](https://img.shields.io/badge/docs-online-blue.svg)](http://docs.racket-lang.org/extensible-parser-specifications/)

extensible-parser-specifications
================================

Like https://github.com/AlexKnauth/seq-no-order, but provides global
constraints.  AlexKnauth's implementation allows dotted "rest" pattern which
this implementation does not support.

This package is unstable, and its API may change at any moment. Once it is
stable enough, it may be merged into [Alex Knauth's
implementation](https://github.com/AlexKnauth/seq-no-order).

The following pattern expanders (and mixin expanders, see the docs) for `syntax/parse` are defined:
* ~seq-no-order
* ~no-order
* ~post-check
* ~post-fail
* ~nop
* ~optional/else
* ~global-or
* ~global-and
* ~global-counter

