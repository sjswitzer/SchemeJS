Thoughts abut "let" and scopes...

Now that the core is becoming essentially a JS AST, "let" needs revisiting.

Goals: deal with both "interactive let" where there's no foreknowledge of subsequent lets and with "compiled" let where references to subsequent lets are illegal.

Sketch: any block potentially begins a scope, as in JS.
The body of a function is a block which includes the parameters.
"Begin" is renamed "block" and is a macro that expends to either "seq" or "blockscope" depending on whether it directly contains any "let" (or "use-ns") forms, ensuring that no two lets are for the same variable. "let" adds the variable (and its computed value) to the current scope. "use-ns" adds the namespace to the current scope.

It's less clear what to do with non-directly-contained lets. They aren't really legal JavaScript.

In this notation, things between <...> are the unbound exportedAPI names of the primitives. So <_let> corresponds to the value of "_let" in the globalScope.
(Primitives that correspond to JS keywords (let_, return_, break_) are those keyword names followed by an _. Those that clash (_eval, _apply), begin with an underscore.

What I'm thinking is that the macro <block> rewrites:
  (<block> ...a...
    (<let_> varname1 ...form... val-form1) ...b...
    (<let_> varname2 ...form... val-form)) ...c...)
to:
  (<blockscope> [
    (varname1 ...a... ...form... val-form1)
    (varname2 ...b... ...form... val-form2)]
    ...c...)
if there are are any top-level "lets_" and otherwise to a "seq,"
leaving any non-top-level <let_> forms alone. These will implement a "dynamic let," which is not a JavaScript thing, per se, but an extension useful for languages that have dynamic scopes.

<function_> is a macro that validates there are no parameter/varname dups clashes and rewrites
  (<function_> [params]
    ...a...
    (<let_> varname1 ...form... val-form1) ...b...
    (<let_> varname2 ...form... val-form)) ...c...)
  )

into

  (<function_scope> [params] [
    (varname1 ...a... ...form... val-form1)
    (varname2 ...b... ...form... val-form2)]
    ...c...)
  ])

The idea being to erase any top-level let_ expressions. Any that remain can
interpreted dynamically, if the client language chooses to use that. It's a it of extra complication, but it allows for the faithful representation of javascript
function scopes, which encompass the params and top-level lets.

Namespaces are string-keyed entries in the scope. "use-ns", again, adds the namespace to the scope. So, for instance, (use-ns .web-gfx) resolves globalScope["web-gfx"] then adds it to the current scope so that, say, "move-to" resolves to .web-gfx.move-to.
The mechanics of representing and using namespaces seems clear enough: first, a variable
(currently termed "Atom") is resolved to an object, then string-valued names are used to resolve the item.
The mechanics of defining namespaces is less clear, much less to notations for doing so in Scheme. For the first, I'll need do do some experiments and for the second I'll have to understand Clojure better.

"Dynamic scopes" come at some cost in complexity and potential fragility and don't currently work quite right in the face of modification. It's probably a good idea to get rid of them since they don't map to JavaScript. They were a fun experiment but should probably be abandoned.

Clojure question: is there something analogous to "(set)" or is there only "(setq)"? Because it would be great to be rid of "(set)" (setting a runtime-computed variable).

