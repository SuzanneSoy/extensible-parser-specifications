[![Build Status,](https://img.shields.io/travis/jsmaniac/extensible-parser-specifications/master.svg)](https://travis-ci.org/jsmaniac/extensible-parser-specifications)
[![Coverage Status,](https://img.shields.io/codecov/c/github/jsmaniac/extensible-parser-specifications/master.svg)](https://codecov.io/gh/jsmaniac/extensible-parser-specifications)
[![Build Stats,](https://img.shields.io/badge/build-stats-blue.svg)](http://jsmaniac.github.io/travis-stats/#jsmaniac/extensible-parser-specifications)
[![Online Documentation,](https://img.shields.io/badge/docs-online-blue.svg)](http://docs.racket-lang.org/extensible-parser-specifications/)
[![Unmaintained as of 2017 because syntax-parse is ill-fitted for this use,](https://img.shields.io/badge/maintained-no%20as%20of%202017%20%28syntax--parse%20is%20ill--fitted%20for%20this%20use%29-red.svg)](https://github.com/jsmaniac/extensible-parser-specifications/issues)
[![License: CC0 v1.0.](https://img.shields.io/badge/license-CC0-blue.svg)](https://creativecommons.org/publicdomain/zero/1.0/)

extensible-parser-specifications
================================


Caveat: the mixins defined with `define-eh-alternative-mixin` cannot be
provided and used in a separate module. Unfortunately, I cannot think of an
acceptable fix for this problem, as solving this would require extracting
parts of the mixin while preserving the bindings of some identifiers, but
altering the bindings of others. This means that for the foreseeable future,
once a mixin is defined, can only be used via `~mixin` (or by directly
invoking it) within the same module.

The regular and splicing syntax classes defined with `#:define-syntax-class`
and `#:define-splicing-syntax-class` will work fine across module boundaries,
however. Manually defined syntax classes, splicing syntax classes or
ellipsis-head syntax classes will also work fine across module boundaries,
even if they contain uses of `~no-order` and `~seq-no-order`, and even if
those special forms contain uses of mixins defined within the same module. In
other words, as long as a definition of a mixin and all its uses via `~mixin`
are within the same module, everything else should work without surprises.

----------

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

