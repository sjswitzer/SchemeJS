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

Over time, compatibility with SIOD has seemed less less feasible and less attractive.For one thing,
SIOD's notion of strings and interoperability goals are very C-centric.
Compatibility with Clojure seems more practical and more useful,
and the implementation has been moving in that direction. The current aim is to support Clojure
fully as possible in a JavaScript-based execution environment instead of a JVM environment.

Obviously some new naming is going to be required and the documentation, such as it is, will need a thorough scrub.

Since SchemeJS compiles (and JITs) into JavaScript and JavaScript's
execution model so closely matches Scheme's, it's a surprisingly fast Scheme implementation.
I venture to say that _much_ more work has been put into optimizing JavaScript's performance
than on any Lisp's.

## Demos

Run a demo by typing going to https://sjswitzer.github.io/SchemeJS/ and typing
`(load "demo.scm")` or `(load "gfxdemo.scm")` or `(load "mediademo.scm")`.

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
with few lines of code or declarations at most. The Node.js REPL and Web REPL are examples of such embeddings.

SchemeJS has _no dependencies whatsoever_ beyond a modern JavaScript implementation.

Since SchemeJS can invoke any JavaScript function, all you need to do to make a function
available to SchemeJS is to add it to Scheme's "global scope": one line of JavaScript.
Objects from JavaScript's globalThis scope are automatically available to Scheme,
and most other functions can be reached by namespace accesses.
For instance, since Date is in the globalScope, `(Date.now)` invokes `Date.now()`.

### Lazy Evaluation of Iterators

JavaScript iterators appear as lists to SchemeJS and the iterable items are only fetched as needed.

Additionally, the lazy-map operation only applies the mapping function when that item is demanded.
Whether the Scheme "map" function is lazy or not is up the the client, who has complete control over
how core operations are bound to Scheme functions.

### List Abstraction

Like all Lisps, SchemeJS has a notion of "lists." In SchemeJS, a list is an abstraction and a protocol for objects with a sequential nature.
It's very similar to Clojure's "sequence" abstraction and will probably be renamed
"sequence" to avoid terminology clashes.

All builtin JavaScipt iterables, excluding String, are lists and every generator instance is a list. And any iterable can be viewed as a list.

The principal difference between a list and a sequence is that iterators
have state, and "next" changes the state of the iterator and returns the next item in the collection.
Lists, however, are stateless. The first item of a list is always the same
item, as are the second and subsequent items. Accessing [REST] on a list doesn't change the state of the list. Accessing rest on that item will
return the same item again.

"Pairs" (cons) calls construct lists, but any object that implements the list protocol is a list and Pairs (cons cells) are in no way special.

Arrays, being built-in iterables, are lists and are the preferred list
representation in the core. The only case where an array cannot used
where other list (sequence) types can is in the specification of evaluation
"forms" in function definitions. Arrays are understood as Array literal
specifications where any other list is understood as a form to be evaluated.

### Compiler and JIT

SchemeJS has both an explicit compiler and a JIT that can be invoked after a client-selectable number of
invocations of a function. The compiler produces perfectly ordinary and performant JavaScript code.
The JIT produces the same code but it is entered through a "guard" that ensures none of the
bound references have changed (the definition of `+` could have been changed by the user).

### Macros

Macros are expressions that can modify or wholly create the code to be executed or compiled in their stead.

There are two kinds of macros:
- Conventional macros (MACRO_TAG), which are forms which, when encountered in the function position of a form are are evaluated with the form's parameters and return a new form (or macro) to replace the macro's form.
- Evaluated parameter macros (EVALUATED_PARAMETER_MACRO_TAG) which, when encountered in a form, where an evaluated parameter is expected, return a pair of two values: an list of forms to be evaluated and whose list results are each to be inserted into the argument list and the replacement of the remainder of the argument list.

(Note: )Whether the distinction between evaluated and unevaluated parameters is needed or not, whether the difference should be conveyed to the macro via a parameter, or whether there should be three kinds of parameter macros, evaluated, unevaluated and unevaluated parameter macros, and whether the head position of a form should be considered a parameter is still in flux and will be decided by further experience. I currently prefer not to use a parameter because it's too easily ignored by mistake, but beyond that I have no further thoughts except that evaluated parameter macros seem good enough for now)

### Unicode

SchemeJS identifiers can use any Unicode "alphabetic" characters (including ideographs).
People should be able to program in their own languages.

Because the Unicode codepoint space is large, this comes at a very small cost in startup time
(I've never noticed it) and a larger cost in memory footprint.
By default, SchemeJS loads the Unicode Basic Multilingiual Plane, but can be opted up to
include the supplemental planes or down to just Latin-1.

## But Wait -- There's Less!

Along the way, and with a bit of prodding from a friend, it occurred to me that the core
of the implementation is essentially an Abstract Syntax Tree (AST) for JavaScript programs.
An AST that is executable, even without compilation, and can be compiled or JITted into JavaScript
at runtime. It's a powerful toolkit for implementing languages and, in particular,
Domain-Specific Languages (DSLs) on top of JavaScript.

(I fully expect someone to eventually write a JavaScript decompiler to SchemeJS that allows
arbitrary program transformations and can then be compiled back into JavaScript.)

The core of SchemeJS hardly knows it is implementing Scheme; all Scheme bindings are in
a separate module. A web interface module implements primitives (and optional language bindings)
for the CanvasRenderingContext2D interface and, soon, for HTML DOM manipulation.

## License

Copyright 2021 Stan Switzer -- (sjswitzer [at] gmail [dot] com)

This work is licensed under a Creative Commons Attribution-ShareAlike
4.0 International License. https://creativecommons.org/licenses/by-sa/4.0/