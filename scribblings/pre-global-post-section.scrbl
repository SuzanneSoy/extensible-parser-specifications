#lang scribble/manual

@title{Post operations and global operations}

Pre operations happen before the @racket[~!] backtracking cut, so they can
affect what combination of alternative clauses the parser will choose. Post
operations happen after the @racket[~!] backtracking cut, and can only reject
the @racket[~no-order] or @racket[~seq-no-order] as a whole (i.e. different
orders will not be attempted after a @racket[~post-fail]. Global operations will
always succeed.

Post operations can access the attributes defined by global and pre operations
as well as attributes defined by the alternative clauses. Global operations
cannot access the attributes of post operations, and pre operations cannot
access the attributes of global and post operations. See
@secref["Order_in_which_the_attributes_are_bound_for_post_operations_and_global_operations"
        #:doc '(lib "extensible-parser-specifications/scribblings/extensible-parser-specifications.scrbl")]
for more details.

@include-section{pre.scrbl}
@include-section{global.scrbl}
@include-section{post.scrbl}
@include-section{pre-global-post-order.scrbl}
