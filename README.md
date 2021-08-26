# SchemeJS: Scheme in JavaScript

JavaScript has dynamic typing, functions, closures, and a JIT with
a small army of crack programmers optimizing its performance.
It’s an ideal runtime for Scheme.
All it lacks is cons cells, an s-expression parser/printer,
eval/apply, and a handful of Lisp primitives.

And a compiler and JIT that compiles Scheme down to perfectly ordinary
JavaScript functions that JavaScript JITs can go to town on.

SchemeJS fills that gap.

It’s hard to keep JavaScript objects from sneaking into the
SchemeJS world, so I decided to invite them in as first-class.
SchemeJS is fully Scheme _and_ fully JavaScript.

This implementation originally aimed for broad compatibility with SIOD, but inevitably,
and in the grand tradition of lisp implementations, introduces a new dialect.
Which is a sin but not a crime.

Run a demo by typing going to https://sjswitzer.github.io/SchemeJS/ and typing
`(load "demo.scm")` or `(load "gfxdemo.scm")` or `(load "mediademo.scm")`.

Since SchemeJS compiles (and JITs) into JavaScript and JavaScript's
execution model so closely matches Scheme's, it's a surprisingly fast Scheme implementation.
I venture to say that _much_ more work has been put into optimizing JavaScript's performance
than on any Lisp's.

## Key Features

### JavaScript Integration and Interoperability

Every SchemeJS function, _whether compiled or not_, is a JavaScript function and can be
invoked directly, with no bridging APIs, from JavaScript. They are perfectly ordinary
JavaScript functions. Any JavaScript function can be invoked directly from SchemeJS as if it
were an ordinary Scheme function.

SchemeJS supports Array and Object literals and every Array, generator (function*) and built-in
iterable object (except String) appears as a "list" to SchemeJS functions. SchemeJS lists are
iterable "for (let item of list) ..." in JavaScript and JavaScript iterables are lists to SchemeJS.

### Optional and Rest Parameters

SchemeJS has notations for optional and "rest" parameters, just like JavaScript.

### Partial Function Application

Any function, whether SchemeJS or JavaScript, when invoked with fewer than its required
number of arguments results in a closure that binds the provided arguments.

### N-Ary Operations and Comparisons

Most operations take an arbitrary number of parameters.
`(+ 1 2 3 4 5)` adds all the numbers and `(< 1 2 3 4)`
returns "true" because each number is less than the next.

### Predicate Conditioals

Predicates such as `boolean?`can be used
as simple predicates `(boolean? obj)` to test if the object is a boolean,
or as conditional branches `(boolean? obj (if-true-expr) (if-false-expr))` since you were
probably going to write `(? (boolean? obj) ((if-true-expr) (if-false-expr))` anyway.

Because the expression `(< 10)` is a closure that returns true
if 10 is less than its argument, the expression
`(filter (< 10) list)` returns a new list with elements from `list`
that are ten or greater. (Seems backwards, perhaps, but that's Scheme for ya.)

### Generalized Special Evaluation

A function defined as `(lambda# 2 (a b c d) forms...)` takes 4 parameters, two of which
are evaluated. This simplifies writing special forms since the interpreter and compiler
can do most of the work of expression evaluation for you.

### Easily Embeddable and Extensible

SchemeJS is a JavaScript module that can be embedded in any JavaScript program or web page
with a half-dozen lines of code at most. The Node.js REPL and Web REPL are examples of such embeddings.

SchemeJS has _no dependencies_ whatsoever beyond a modern JavaScript implementation.

Since SchemeJS can invoke any JavaScript function all you need to do to make a function
available to SchemeJS is to add it to Scheme's "global scope: one line of JavaScript.
If you want special handling by the compiler that can also be arranged, but it's a bit more involved.

### Lazy Evaluation of Iterators

JavaScript iterators appear as lists to SchemeJS but the iterable items are only fetched as needed.

Additionally, the lazy-map operation only applies the mapping function when that item is demanded.
Whether the Scheme "map" function is lazy or not is up the the client, who has complete control over
how core operations are bound to Scheme functions.

### Compiler and JIT

SchemeJS has both an expliit compiler and a JIT that can be invoked after a client-selectable number of
invocations of a function. The compiler produces perfectly ordinary and performant JavaScript code.
The JIT produces the same code but it is entered through a "guard" that ensures none of the
bound references have changed (the definition of `+` could have been changed by the user).

### Macros

Coming soon!

### Unicode

SchemeJS identifiers can use any Unicode "alphabetic" characters (including ideographs).
People should be able to program in their own languages.

Because the Unicode codepoint space is large, this comes at a very small cost in startup time
(I've never noticed it) and a larger cost in memory footprint.
By default, SchemeJS loads the Unicode Basic Multilingiual Plane, bt can be opted up to
include the supplemental planes or down to just Latin-1.

## But Wait -- There's Less!

Along the way, and with a bit of prodding from a friend, it occurred to me that the core
of the implementation is essentially an Abstract Syntax Tree (AST) for JavaScript programs.
An AST that is executable, even without compilation, and can be compilled or JITted into JavaScript
at runtime. It's a powerful toolkit for implementing languages and, in particular,
Domain-Specific Languages (DSLs) on top of JavaScript.

(I fully expect someone to eventually write a JavaScript decomppiler to SchemeJS that allows
arbitrary program transformations and can then be compiled back into JavaScript.)

The core of SchemeJS hardly knows it is implementing Scheme; all Scheme bindings are in
a separate module. A web interface module implements primitives (and optional language bindings)
for the CanvasRenderingContext2D interface and, soon, for HTML DOM manipulation.

## License

Copyright 2021 Stan Switzer -- (sjswitzer [at] gmail [dot] com)

This work is licensed under a Creative Commons Attribution-ShareAlike
4.0 International License. https://creativecommons.org/licenses/by-sa/4.0/