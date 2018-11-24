# A full specification

Here's a specification, in Pantagruel, of Pantagruel's binding rules.
```pantagruel
eval (p: Program) :: Bool
Program <= [Section]

" A section head must have at least one statement; a section body can be empty.

section (head, body: Head, Body \ #head > 0 ) => Section

Head <= [Comment, Declaration, Alias]
Body <= [Comment, Expression]
Comment, Declaration, Alias, Expression <= [String]
---
eval p <- all sect from p \ is_bound? sect

;

is_bound? (sect: Section) :: Bool
---
" All variables referred to in a section head must be defined by the
" end of that section head. All the variables in a section body, however,
" must be defined by the end of the *next* section body.

is_bound? sect <-                                           ...
    (all h from sect.head \ all sym from h \ is_bound? sym) ...
    and                                                     ...
    (all b from (p ((p sect) - 1)).body \ all sym from b \ is_bound? sym)

;

is_bound (sym: String) :: Bool
---
is_bound sym <- (sym from env p (p sect)) or (sym from init_scope)

;

env (p: Program) :: [Scope]
init_scope() :: Scope
Scope <= {String}
```

## Exploration

Let's consider some of the features.

In the head of the first section (the text before the first `;`, which
acts as a section separator), we see one function declaration, one
constructor declaration, four domain aliases, and one comment. The comment
is not interpreted by `pant` and has no semantics in the language.

The function declaration `eval (p: Program) :: Bool` introduces a
function, `eval`, which takes a `Program` `p`[^1], and returns a `Bool`.

[^1]: `p : Program`, pronounced by itself as "p in Program", indicates
that `Program` is *the domain of* `p`. That is, `Program` is the set of
all possible programs and `p` is any element of that set. In this way
domains are analogous to types but more powerful; they can also include
restrictions on the values within the type, such as `x != 0` or `x mod 2
= 0`.

The constructor declaration `section (head, body: Head, Body \ #head >
0 ) => Section` introduces a function `section` which takes a `Head` and
`Body` and produces a `Section`. There's also a single precondition to
the constructor, introduced by `\`, which says that the size of `head` has
to be greater than 0, ie, there needs to be at least one element in it.

Notice at this point that we've referred to several variable domains
before defining to them: `Program`, `Head`, `Body`. That's fine, as long
as they are defined by the time we get to the end of this section head,
which happens after a brief comment.

The domain aliases all have the form of `Bar <= [Foo]`, `Baz <= [Foo,
Bar]`, or `Bar, Baz <= [Foo]`.

In each case they introduce a new domain or domains on the left side,
and define this domain as shorthand for some more complex domain on the
right side. For convenience's sake, we can introduce multiple domains
at once if they all refer to the same thing. So `Comment, Declaration,
Alias, Expression <= [String]` introduces `Comment`, `Declaration`,
etc. and aliases them all to `[String]`. In other words, each of those
is simply a list of `String`s.

There are only two basic types of *containers* in Pantagruel: sets and
lists. A domain such as `[X]` refers to *a list of `X`s*, whereas `{X}`
refers to *a set of `X`s*. Lists and sets can also be used with actual
values like variables and literals. The only semantic difference between
the two is that sets have no ordering.

Finally, a domain container with multiple elements `A`, `B` can be
understood as a container whose elements are either `A` or `B`. So `Body
<= [Comment, Expression]` says that `Body` is a list whose elements
are all either `Comment`s or `Expression`s.

By the end of the first section head, everything referred to has been
formally introduced; either as the name in a function declaration,
the arguments in a function declaration, or the left side of a `<=`
expression. The two exceptions are `Bool` and `String`; these
domains are predefined in Pantagruel.

The body of the first section has a single statement, which is a
*refinement*. It says that `eval p` is refined by `all sect from p
. is_bound? sect`.

The right side of that statement is a universal quantification, which will
be easier to see when we see Pantagruel's pretty-printed version. It can
be read, "for each `sect` which is an element of `p`[^3], `is_bound? sect`
should be true". `is_bound? sect` is a function application; `is_bound?`
has not been defined yet, and won't be before the end of this section,
but that's alright.

[^3]: In this case, `p` is a `Program`, and since a `Program` is a list of
`Section`s, it follows that each `sect` is a `Section`.

We begin a new section with `;`, and the second section's head
defines `is_bound?` for some `Section` `sect`. It's important that
we've introduced this argument `sect`; even though we were able to say
something about `all sect from p` earlier, that doesn't introduce `sect`
into the scope of the program as a whole.

The body of the second section consists of a relatively long refinement
of `is_bound? sect`. Because it extends onto more than one line, we
continue the line with `...` until we're done with the expression. The
refinement of `is_bound?` consists of evaluating both halves of a
logical expression and testing whether both are true. The first half
is a nested quantification, where the expression after the first `\`
is a second quantification that makes use of the element introduced in
the first. In other words, "for each `h` in `sect.head`, for each `sym`
in `h`, `is_bound? sym` should be true". The second half is constructed
similarly, but instead of `sect.head` the set we're drawing from is `(p
((p sect) - 1)).body`. Let's break that down.

The first expression to be parsed is the innermost parenthetical, `(p
sect)`. This is the application of `p` as a function to `sect`. Since
`p` is a sequence and `sect` is an element of that sequence, we can
understand `p sect` to refer to the index *i* of `sect` within the
sequence `p`. That expression is inside a parenthetical `(p (i - 1))`,
so we parsed that as applying `p` to `i - 1`. `p` is a list, and we can
understand the application of a list to an integer to be indexing into
that list. Therefore `(p ((p sect) - 1))` means "the section in `p`
one before `sect`" (we can call it `p'`). Having evaluated everything
within parentheses, we end up with `p'.body`. Dots allow object-style
suffix function application/attribute reference, so that is parsed as
"the `body` of `p`".

Since `Section` was introduced with a type constructor `=>`, we can use
the arguments to that constructor for convenient field-style access. So
`sect.head` is the `head` of `sect`, and `(p ((p sect) - 1)).body`
is the body of the *previous* section to `sect`.

Thus we have a somewhat formal expression of the binding rules in
Pantagruel: every symbol used in a section head must be bound by the end
of that section. However, variables can be referred to in a section body
and only defined in the section following.

Using this refinement structure we can write our specifications as a
series of elaborations gradually increasing in detail.

Finally we have to define what it means for a symbol to be
bound. `is_bound sym <- (sym from env p (p sect)) or (sym from init_scope)`
expresses that `is_bound` for some String `sym` is refined by checking
whether `sym` is an element of `env p` at `p sect` or if it's an element
of `init_scope`.

`env` and `init_scope` are new concepts and need to be defined. The
program *environment* in Pantagruel consists of a sequence of binding
scopes, one for each section, into which symbols are inserted when
they're formally defined. Pantagruel also contains an *initial scope*,
where things like `String` and `Bool` are predefined.

More can be said about the binding behavior---for instance, how are
symbols inserted into program environments?---but in this specification
we're more interested in communicating the rules about when things need
to be bound. So we do the bare minimum, and introduce the functions and
domains so as to give a sense of their signature, but don't bother to
say anything more detailed. We intentionally remain a bit vague about
`env`.

The fact that the syntax and binding rules of Pantagruel are enforced by
the `pant` program, but that the semantics are generally ad-hoc and
to be understood by convention rather than axiom, allows us to exercise
a *gradual specification* where we can be precise about the things we want
to be precise about, and vague (but explicitly so) about the things we
don't want to dwell on.

## Program output

The `pant` interpreter will evaluate a Pantagruel program, checking
that symbols have been defined according to the rules described here,
and then print out a formatted version of the section.

When the above program is put into a text file called *binding.pant*,
and we run `pant binding.pant`, this is what's output:

-----
-----

eval(p:Program) ∷ 𝔹  \
Program ⇐ [Section]

> A section head must have at least one statement; a section body can be empty.

section(head, body:Head, Body ⸳ #head > 0) ⇒ Section  \
Head ⇐ [Comment, Declaration, Alias]  \
Body ⇐ [Comment, Expression]  \
Comment, Declaration, Alias, Expression ⇐ [𝕊]  \
eval p ← ∀ sect ∈ p ⸳ is-bound? sect

***

is-bound?(sect:Section) ∷ 𝔹

> All variables referred to in a section head must be defined by the
> end of that section head. All the variables in a section body, however,
> must be defined by the end of the *next* section body.

is-bound? sect ← (∀ h ∈ sect.head ⸳ ∀ sym ∈ h ⸳ is-bound? sym) ∧ (∀ b ∈ (p ((p sect) − 1)).body ⸳ ∀ sym ∈ b ⸳ is-bound? sym)

***

is-bound(sym:𝕊) ∷ 𝔹  \
is-bound sym ← (sym ∈ env p (p sect)) ∨ (sym ∈ init-scope)

***

env(p:Program) ∷ [Scope]  \
init-scope() ∷ Scope  \
Scope ⇐ {𝕊}
