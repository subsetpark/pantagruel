# Pantagruel: An Unambiguous, Undefined Program Specification Language

## What is this?

I had the idea for this language after reading into some of the offerings
currently available in the world of [Formal Methods][FM]: things like
[Z], [TLA+], and somewhat more obscurely, the language developed in [A
Practical Theory of Programming][practheo]. I had and have an interest in
formal methods though my interest, like that of many, is unconsummated,
as I haven't become anything close to expert in any of them and haven't
had the opportunity to use them in a professional setting.

[FM]: https://users.ece.cmu.edu/~koopman/des_s99/formal_methods/

[Z]: https://staff.washington.edu/jon/z-book/index.html

[TLA+]: http://lamport.azurewebsites.net/tla/tla.html

[practheo]: http://www.cs.toronto.edu/~hehner/aPToP/

The desire to put together something of my own came after reading
*aPToP*. Hehner's rationale for formal methods is, paraphrased, is:
programming would be better if program specifications had two closely
related qualities that they currently lack:

1. Unambiguous in meaning
2. Provably correct

To that end he puts together a language of program behavior which is
as unambiguous in its meaning as mathematics with a set of axiomatic
equalities that allow the programmer to express a precise and complete
specification of behavior and then refine it, axiomatically, to arrive
at a provably correct implementation.

I'll leave the reader to decide for themselves whether Hehner's offering
is practicable, but I found myself inspired. I lack the brainpower to
write a formal method that, along the lines of Z and TLA+, allows us
to express program behavior according to some axiomatic theory. So this
is not an attempt to replace or even mimic those systems. However, the
notion of a more mathematical and therefore *less ambiguous* notation
for program behavior is appealing. The observation that English words
like *should*, *all*, *none of* and so forth can contain a multiplicity
of overlapping meanings is well-taken.

What would be especially nice is if we had a notation that was more
mathematical but also easily written by hand, and intuitive to the
programmer, so we could use it as a lingua franca for whiteboarding,
ideation in notebooks, and the like. There's a possibility for something
with a much lower barrier to entry (for this admittedly more modest
goal) than something like Z. Part of what makes this possible is that we
can jettison a formal *semantics* entirely. It doesn't need to *mean*
anything. At the end of the day the only semantics will be in the mind
of the human reader. So we can try to provide them with a language that
they can use to be as descriptive as is necessary, but only for their
own purposes.

Of course, any language with an undefined semantics will not be *actually*
unambiguous in any objective way. But we might be able to at least avoid
certain classes of less obvious ambiguity.

In other words, it might be nice if we had a system that enforced
certain constraints about exhaustiveness and form while still allowing
the author to be as lazy and hand-wavy as they want if they decide it's
not important to be more specific. There might be a value in forcing
ambiguity into sentences like 'There is a function *f* (and I won't
say anything else about it)' rather than 'When the program is finished,
all mailboxes should be empty'.

To that end the last (and only) feature of Pantagruel is that it can be
parsed by a computer program with a well-defined semantics of *binding*
and it can enforce certain constraints on binding. That is, it can
force the user to leave no symbol undefined, even if the definitions
entered are nonsensical or wrong. The hypothesis of Pantagruel is that
there is a cognitive benefit to requiring this kind of exhaustiveness,
even if there are no constraints placed on what is said.

## A sample program

Here's a trivial but complete Pantagruel program, translated from an example from *The Way of Z*.

```pantagruel
user() => User
doc (owner: User) => Document

" A specification for a small document management system.

check_out (u, d: User, Document)

" A user may check out a document if they have permission to access it
" and it's not currently checked out.

(d.owner = nobody and (has_perm? u d) and check_out u d) -> d'.owner = u
(d.owner != nobody or ~(has_perm? u d)) -> d'.owner = d.owner

;

nobody () :: User
has_perm? (u, d: User, Document) :: Bool
```

Paragraph 1: The introduction of our two domains, *User*s and *Document*s.

P. 2: A comment introducing the program.

P. 3: The introduction of a procedure, *check-out*, which takes *u*,
a *User*, and *d*, a *Document*.

P. 4: A comment describing our specification in natural language.

P. 5: Two propositions describing the expected state after the
*check-out* procedure. The first says that if the owner of *d* is
*nobody* and *has-perm? u d* is true and *check-out u d* is evaluated,
then the successor to *d* (that is, *d* at some next point in time) will
have an owner of *u*. The second line says if either of the two first
conditions is otherwise, the successor to *d* will have the same owner
as *d* does. In this line we have left out the bit about *check-out u
d* because in our universe, that proposition is true whether or not we
run *check-out*.

`;`: Pronounced "where", acts as a section separator.

P. 6: "Introduces" the two procedures referred to in the previous section,
*nobody* and *has-perm?*. *nobody* is a procedure which yields a *User*,
in this case understood to represent a document with no owner. *has-perm?*
is a procedure which will return a Boolean value indicating whether a user
has permission to check out a document or not; in this specification
we've left it at that. In a different document or if we were also
interested in the specifics of who can check out a document and when,
we could continue the specification with statements about *has-perm?*.

Pretty printed, that's:

----------------
----------------

user() ⇒ User \
doc(owner:User) ⇒ Document

> A specification for a small document management system.

check-out(u, d:User, Document)

> A user may check out a document if they have permission to access it
> and it's not currently checked out.

(d.owner = nobody ∧ (has-perm? u d) ∧ check-out u d) → d'.owner = u \
(d.owner ≠ nobody ∨ ¬(has-perm? u d)) → d'.owner = d.owner

***

nobody() ∷ User \
has-perm?(u, d:User, Document) ∷ 𝔹

---------------
---------------

The document is structured the way it is so that we are able to express
what we take to be the gist of our program in as terse a manner as
possible. In this case we want to lay out unambiguously the relationship
between ownership, check-out permissions and the effects of checking
out a document. The `pantagruel` program is not terribly concerned with
the specifics of what we say and trusts us to communicate what we need
to. However, it wants to keep us honest, and thus we are constrained
to define all of our terms to at least some degree. For instance, if we
had included the expression `d.owner = nobody` in the first section but
not declared the `nobody` procedure in the second, barebones as it is,
`pantagruel` would raise an error upon evaluating the program.

## Who would be interested in this?

It's not clear, aside from "me". My hypothesis is that this will be
interesting and maybe even useful to anybody who a) has an interest in
formal methods or formal reasoning but b) is not working on projects of
sufficient seriousness, or is not sufficiently clever and mathematically
literate, to be using proper formal methods. It seems to me that it will
be useful to have a well-defined language for communicating algorithms to
oneself and others, and that if we can exploit its definition to subject
it to automatic analysis and impose certain lightweight constraints, all
the better.

To that end, the language itself needs lots of battle testing; lots
of thinking or whiteboarding done in it, to see what is still awkward
to express, what constructs are unnecessary or redundant. At the same
time I will continue to work on `pantagruel` the computer program, to
build something that is capable of providing some amount of value given
a Pantagruel text file.

# Installation

If [available in Hex](https://hex.pm/docs/publish), the package can
be installed by adding `pantagruel` to your list of dependencies in
`mix.exs`:

```elixir
def deps do
  [
    {:pantagruel, "~> 0.1.0"}
  ]
end
```

Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc) and published on [HexDocs](https://hexdocs.pm). Once published, the docs can be found at [https://hexdocs.pm/pantagruel](https://hexdocs.pm/pantagruel).

