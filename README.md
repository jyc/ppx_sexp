# ppx_sexp

ppx_sexp is a ppx preprocessor for embedding S-expressions in OCaml programs.

For example, this:

    [%sexp (define a "hi there!")]

is translated into:

    `List [`Symbol "define"; `Symbol "a"; `String "hi there"]

# Why is this useful?

Well, Scheme has a very cool set of specifications and tooling called [SXML](https://en.wikipedia.org/wiki/SXML) that lets you write XML documents using S-expressions.
With some specific serialization rules, you can serialize SXML to HTML.
This is incredibly powerful in Scheme particularly because you can then use quasiquotation to seamlessly integrate macros, procedures, variables, etc. inside of documents, for example to render a website.
Quasiquotation allows you to write lists constants and unquote expressions inside.
In effect, SXML provides you a templating language with the full power of Scheme (and one that is a lot cleaner than XML and other templating languages, in my opinion).
You have lexical scoping, can dynamically generate subtemplates (no need to use a new import or expression system just for templates), etc.

Unfortunately, OCaml doesn't have quotation/quasiquotation syntax.
Whereas in Scheme you could write a HTML document that shows a user's name like this: (note that `,name` is the syntax for quasi-unquotation)

    (html (@ (lang "en"))
      (head
        (title "Hi there!"))
      (body
        (h1 "Hello, " ,name "!")
        (p "Great to see you.")))

In OCaml you end up with a lot more noise, because there is no way to enter a mode in which 'symbols' are quoted by default.
Here we use a `\`Y` variant to identify symbols, an `\`S` variant to identify strings, and an `\`L` variant to identify lists:

    [`Y "html"; `L [`Y "@"; `L [`Y "lang"; `S "en"]];
     `L [`Y "head";
         `L [`Y "title"; `S "Hi there!"]];
     `L [`Y "body";
         `L [`Y "h1"; `S "Hello, "; `S name; `S "!"];
         `L [`Y "p"; `S "Great to see you."]]]
        
This is very ugly.

With the ppx_sexp syntax extension, we can regain much of the expressiveness of S-expressions and quasiquotations inside of OCaml:

    [%sexp
       (html (+ (lang "en"))
          (head
             (title "Hi there!"))
          (body
             (h1 "Hello, " [%in name] "!")
             (p "Great to see you.")))]

Note that `[%in name]` is used to unquote the `name` variable.
To be properly typed, it would have to be of the form `\`String "Some Name"`.

# Limitations

ppx preprocessors accept an AST as input and return a mutated AST.
As a result, the "S-expressions" inside of `[%sexp ...]` must actually be valid OCaml syntax.
This works for most S-expressions -- for example, OCaml sees `(title "Hi there!")` as the application of a procedure identified by `title` to the string constant "Hi there!".
However, you **cannot use OCaml keywords inside of ppx_sexp expressions**.
Furthermore, parentheses do not themselves make a list -- they can only give applications precedence.
For example, you sadly cannot write:

    [%sexp
       (let ((a 8))
          (/ a 2))]

First, `let` is an OCaml keyword, so the parser will throw a syntax error even before the AST is passed to ppx_sexp.
Second, `((a 8))` is in OCaml syntax identical to `(a 8)`, so that is what ppx_sexp will see in the AST it is given.
(To express this, you could write `([a 8])` or `[(a 8)]`.)
Third, `(/ a 2)` will trigger an unmatched parentheses syntax error.

These are pretty ponderous restrictions if the goal is to embed Scheme code in OCaml.
However, my goal in writing this extension was allow for SXML-like expressions.
HTML documents do not often include `let` or `/` tags, so ppx_sexp works well enough for this use-case.

# Installation

You can use the included `./install` script to install ppx_sexp using ocamlfind.
`./uninstall` will uninstall ppx_sexp similarly.
