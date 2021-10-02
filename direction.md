Issues about the direction of this project:

1) Is it to create fluent a Lisp/Scheme interface to JavaScript/HTML, following thet wherever it leads, or

2) Since it seems to be evolving toward Clojure, at least in spriit, a "better"--for some value of "better"--version of Clojure for JavaScript. At least that has a user community.

3) But if Clojure is the direction, how much fidelity to Clojure is required? What precicely would be the goals of that effort? Supporting n% of existing Clojure libraries? What is n? Even now, Graal support is apparently an issue for Clojure; surely JavaSctript will be far more problematic. Why get on that treadmill?

4) It's been productive to move in the Clojure direction so far but I'm hitting obstacles. At various points, Clojure made choices to conform to the strictures of the JMV enironment and and as many points I've made choices to conform to the strictures of the JavaScript environment. I'll detail below.

5) The motivation of this project has beeen to explore certain ideas where they naturally lead. And that's lead to some natural convergence to Clojure but also some divergence. But yoking it to Clojure compatibility pretty much kills the "exploration" angle for the sake of the "maybe there will be a community for this project" angle.

6) A Lisp/Scheme with seamless interop with with JavaScript/HTML remains the goal of this project, but that brings conflict--in the details mostly but details matter--with Clojure which has identical goals with Java.

7) What I really want to be doing now is to (1) finish abstracting the core to be a "pure" JS AST, and (2) create natural Scheme/Lisp programming environment for the web (HTML5/JS). This would include, despite my predisposition against it, synergy with the NPM ecosystem.

8) Instead, what I'm doing is puzzling over bridging the gap between what I've envisioned and Clojure. Which is fine, I guess, and maybe necessary if this is to have any purchase, but it's exhausting.

ANYWAY... Some bridges to gap.

Since this was conceived as a JS-hosted Scheme/Lisp, many choices were made in favor of JS rather than Java. And I'm convinced that had Clojure targetted JS initially, it would have made some of these choices differently than it did; perhaps even very much as I have.

- Types and Literals: Numbers, BigInts, Strings, Arrays, Objects.

Types and notations for numbers: JS has Double and BigInt. Clojure adds Decimal (presumably through Java BigDecimal) and Rational (which, I think is a Clojure-specific thing).

As it stands, SchemeJS supports native JS numbers, BigInt, Object, etc. and their literal notations. Rational and BigDecimal are easy to implement.

- Operator polymorphism.
In JS, + adds numbers and concatenates strings (and other stuff besices). This leaks out in the SchemeJS bindings--by design--but it's certainly possible to create different bindings; I've already separated bindings from the operator definitions themselves with this in mind.

Strings are not as problematic as they might be since I gave up on SIOD compatibility a month ago, but bring new preplexities. Both Java and JS use UCS-2 as the underlying representaton of strings and this is helpful but ultimately a wrong choice. Java and Clojure have a "character" type which, sadly, is no such thing but rather a USC-2 codon. JS has no character type at all, which seems wierd to old C programmers but oddly precient; there are only strings of one character. I could go on about this but the thing is that Clojure has a character type and character literals and these fit awkwardly into the JS target environment.

Currently SchemeJS String literals accept JS notations; it could conceivably accept Java notations instead (or in addition). Lurking beneath this is the question of whether SchemeJS continues as a thing in itself or is painted over Gerhard Richter-like with Clojure.

JavaScript Arrays are weird and there is nothing I can ever do to fix that. At the same time the only sensible thing to do is map Clojure vectors onto JS arrays. It will work most of the time, but failure is always just around the corner.

Objects and Maps and their literal notations.

Currently, the SchemeJS "Object Literal" syntax is essentially the JS Object literal syntax. But it's half an iota away from Clojure's Map syntax too. And while a JS Object is map-like, it isn't quite a Clojure map. At the same time JS has a Map (not to mention Set) object correspoiding quite well to Clojure's Map.

The best JS interop story is that the {} syntax represents Bbject literals (and, while it's nopt a hill I'll die on its one I'll defend fiercely), and the best Clojure compatibility story is that it represents JS Map. There really is no good solution for this, just a hard choice leading to a bad syntax for some group of users.

Atoms, Namspaces, Vars, Symbols, keywords, etc.

Need to understand this better. Much better.

Reader Macros.

I'm convinced that some variation on my parameter macro scheme can obviate the need for reader macros. For instance, it can handle "syntax quoting" quite easily. And doing so independent of the parser itself but rather on the "AST" is way more general and more in the spirit of Lisp.

Trouble of my own making.

I'm unhappy with "let." Let both introduces a scope and the bindings in the scope. JS doesn't work that way. "Let" introduces a binding into a function or block scope and these are conceptually separate things. I'll probably let it slide, but it still bugs me a lot.

Immediate to-dos:

Finish trimming the core down to an AST toolkit.

Syntax quoting because macros are a pain without them. Probably in the core because macros are in the core.

Nail down the namespace story and implement in compiler (currently the prototype is implemented only in the interpreter).



