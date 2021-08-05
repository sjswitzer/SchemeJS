# Implementation Notes

## Coding Conventions

Generally, variable, function and class names are camel case. An exception
is when a function is also a Scheme function with dashes in its name. In that case
the function uses an underscore where a dash occurs in the Scheme name.

Constants are all-caps. Strings are generally "str" if they are for humans; and 'str'
if they're for internal purposes; `str` is used when convenient, as it quite often is.

I use "== null" specifically to test for "nullish" (undefined and null). Be careful
with null since it reports its type as "object" but doesn't do objecty things. Otherwise
I use ===.

"Let" is used rather than "var." Var is forbidden.

I prefer to define functions _after_ they're used. Tell the big picture first, then details.
Pascal taught us some bad habits.
Often a function will have a broad API, process its arguments and options then call
a mini-me version of itself defined locally. This gets the grubby stuff out of
the way and then the function can, for instance, recurse on the simpler API and common
arguments don't need to be passed around but instead can be accessed from the containing scope.

I prefer arrow functions for one-liners, regular functions for longer things. for situations
where an own "this" is required, or where a forward reference is needed.

I don't use "const" for things that happen to be const, for instance as loop iterands;
that's just too damn fussy. I use it for things that _should_ be const or _must_ be const.
Sometimes I use const to make it trivial for JITs to discover that sonething cannot change or,
in the case of selectable options like the SchemeJS JIT and TRACE_INTERPRETER, JITs can easily
and dependably DCE the optional code.

Instead of passing in booleans and other unnamed values as parameters (foo(3, true, false, 17))
I generally try to assign the value to a variable so that it has a name then pass that variable.
If a function has more than a few obvious parameters, I usually pass an "opts" object instead
so that parameters can be named entries in the opts argument. Usually, the first thing the function
does is access the options, assign them to variables and provide a default. This gets that
out of the way up front and provides documentation on what the options are.

## Runtime Details

The current scope is simply a JavaScript object and the Scheme scope chain is implemented through its
prototype property. All Scheme code is invoked with "this" as the current scope, and it can
resolve any reference using this[ref]. Nothing could be simpler. Or faster.

Function closures do a lot of the heavy lifting here. For instance, Scheme closures are implemented
as JavaScript closures that capture the scope then apply the function with the captured scope. Scheme
lambdas are also compiled into JavaScript closures. Since a closure is a JavaScript function,
it can be called as an ordinary JavaScript function _whether it is compiled or not_. This is
the general case: Scheme functions can call any JavaScript function and JavaScript can call
any Scheme function.

The Scheme dialect is modelled roughly on SIOD since there is a corpus of existing code for
that dialect, but it could conceivably be extended in different directions.
I _strongly_ prefer not to fork the implementation for dialects, but instead make the dialect an
instantiation option.

It proved difficult to keep JavaScript objects from sneeking into the SchemeJS environment
so I invited them in as first'class. SchemeJS is fully Scheme _and_ fully JavaScript.
It even has notations fof JavaScript Array and Bbject literals. Every JavaScript operator and global
symbol is available in SchemeJS. Cons cells are JavaScript-iterables but internally
I generally avoid iterating them because "while (isCons(obj)) ..." is a lot faster.
But you _can_ iterate over them using "for (let obj of list) ...".
SchemeJS does _not_ see iterables as Cons cells by default. That would be too slow.
But you can create a "list-view" wrapper that lazily invokes the iterator every time
"cdr" is invoked. That works just as well and doesn't tax ordinary list manipulation
primitives.

Scheme atoms are simply Symbols that happen to be in the ATOMS dictionary.

A Scheme instance is the global scope itself. Globally-defined Scheme values and functions
are the values of their Atoms, i.e. Symbols. JavaScript API methods are string-keyed, and
generally must be invoked as globalScope.apiName(), which is pretty much what you'd
do anyway.

The parser provides feedback to REPLs through a parseContext list. REPLs can use the parseContext for
auto-indent and syntax highlighting if they desire. SchemeSJ comes with a Node.js CLI/REPL and
a Web REPL, that are just simple embeddings of the SchemeJS module. All you need to do
is import it and create a global scope instance.
## Compiler

The compiler compiles a "binder" function that creates local variables containing any outside
values and function definitions since code generated by "new Function" can only see the global
scope. The binder is then invoked so that references can be resolved to things present in
the compilation scope. It returns the function itself.

The compiler uses the builtin function definitions themselves as code-generation templates
in most cases. Special forms (with unevaluated arguments), requires a small code-generatin hook.
To see the compiled code, just invoke String(function), or (String function) in Scheme.
Templates are weaved together using something very much like SSA graphs and the "if" hook
basically generates a "PHI" node. The result is JavaScript in 1-1 correspondence
between Scheme and you'd write it in JavaScript yourself, except for a bunch of assignments
to SSA variables. I trust that the JITs flow analysis will erase those variables. Modern
compiles don't really believe in variables anyway; they believe in values. They wouldn't
even use variables if debuggers didn't need them. But, alas, they do.

I digress. The point is that JITs will go to town on the generated code and since
teams of talented engineers have put countless man-years of effort into making JavaScript fast.
I stand atop all that work without hardly lifting a finger. It's interesting to note
the degree of correspondence between Scheme's runtime and a JavaScript runtime.
You couldn't design a better Scheme runtime if you tried:

Dynamically typed: Check.

Garbage collector: Check.

Strings, numbers, booleans and Functions: Check. I threw in BigInt for fun. "Factoral" likes it.

Function are data. Check. You can create a function from a string and get the string body
of a function.

Atoms: Check. They're Symbols (in the ATOMS dictionary).

Scopes: Check. I use the prototype chain and JavaScript resolves it just as it would
any ordinary "this.foo" reference. That is something JITs slave over.

Closures: Check. SchemeJS closures are JavaScript closures. Compiled SchemeJS functions
resolve their scope variables as statically-scoped variables in JavaScript.

Cons cells and nil? Those are really the only things missing and I use JavaScript classes
for those... and threw in lazily-evaluated CAR and CDR references for fun.

(Unfortunately, while JavaScript has an internal set-where-found-in-scope primitive, it doesn't
appear to be accessible to user code in any useful way. This is the _only_ runtime
feature I had to emulate. Real Scheme/Lisp programmers don't use set anyway. But note that the
SchemeJS compiler doesn't need it, so compiled Scheme doesn't suffer any emulation penalty.)

In short, the JavaScript runtime is ideal for Scheme--_much_ better than a Java VM--because
it has dynamic types, the right sorts of primitive types, real closures, no need for "boxing"
(anyway, it's transparent to users and highly-optimized)
and implements something that can be used for scope resolution as a highly-optimized primitive.
All it needs is cons cells, nil, an interpreter, a parser, a "printer", a REPL, a compiler,
and a handful of Lisp primitives. Hence this project. There's no emulation layer because
there's nothing to emulate (save "set/setq").

I didn't set out to a blazing fast Scheme implementation, but halfway into implementing
it I realized it inevitably would be, thanks to the JavaScript runtime and its JITs.
If I could finish it, that is. (And to be fair, the parser could be a _lot_ faster).

Probably the best way to think about it is that JavaScript was secretly Scheme all along,
just as Brendan Eich originally intended. Recent improvements in ES6 and beyond have exposed
more of the underlying Lispyness and this project wouldn't have been attempted without them.

## Future Work

I have plans for a non-recursive interpreter and tail-call-optimization in both the
interpretrer and compiler.

Make let, let* and letrec distinct, I think. They're all implemented as letrec currently.
No good reason to do it except that it could cause some existing SIOD code to break if I don't.
Three different "let" primitives was a mistake from the outset, IMO; all you need is "letrec."

It would be nice to support writing async and generator functions naturally but I haven't
given it much thought beyond the observation that a non-recursive interpreter would make "yield" pretty straightforward. Compiling it would require a different approach, I suspect.
Probably transform the Scheme into a series of continuations and compile those the usual way.
I imagine this is what JavaScript implementations do at the AST level. At least I have the
advantage that Scheme code is an AST inherently.