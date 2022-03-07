# Pantagruel: An Extremely Lightweight Formal Specification Language

Pantagruel is tool for *thought* and *communication*. It's not a programming
language, and it's not a program for verifying other programs. It's a language
designed to help you express your ideas about what programs (or systems, or
algorithms, or bylaws...) should do. It is designed to help you do so in a way
that is terser and clearer than natural language; that's the *communication*
part. The *thought* part comes from the bet that being helped, and sometimes
constrained, to communicate in an unambiguous and slightly more rigorous way
than you might otherwise will assist you in thinking things through.

The first part of Pantagruel is a *program specification language*: a notation
with a defined syntax whose sentences will describe the workings of some
program, to be articulated for the benefit of the people who will implement it,
as well as the people who design it and the people who will ensure it's
implemented correctly.

The second part is the `pant` document checker, which is a program that
understands Pantagruel text documents. `pant` is capable of ensuring that
documents follow certain rules, beyond those of syntax, which are designed to
promote rigor and clarity of thought.

## A sample program

Here's a trivial but complete Pantagruel program, translated from an example
from *The Way of Z*.

```pantagruel
// par. 1
module CHECKOUT.

// par. 2
User.
Document.
owner d: Document => User.

// par. 3
// A specification for a small document management system.

// par. 4
check_out u:User, d:Document.
---

// par. 5
// A user may check out a document if they have permission to access it and
// it's not currently checked out.

// par. 6
owner d = nobody and has_perm? u d -> owner d' = u.
owner d != nobody or ~(has_perm? u d) -> owner d' = owner d.

;

// par. 7
nobody => User.
has_perm? u:User, d:Document => Bool.
---
```

¶ 1: (optional) Module declaration.

¶ 2: The introduction of our two domains, *User*s and *Document*s, and a
procedure that specifies the *owner* relation between the two.

¶ 3: A comment introducing the program.

¶ 4: The introduction of a procedure, *check-out*, which takes *u*,
a *User*, and *d*, a *Document*.

¶ 5: A comment describing our specification in natural language.

¶ 6: Two propositions describing the expected state after the
*check-out* procedure. The first says that if the owner of *d* is
*nobody* and *has-perm? u d* is true and *check-out u d* is evaluated,
then the successor to *d* (that is, *d* at some next point in time) will
have an owner of *u*. The second line says if either of the two first
conditions is otherwise, the successor to *d* will have the same owner
as *d* does.

`;`: Pronounced "where", acts as a section separator.

¶ 7: Provides a gloss for the two procedures referred to in the previous
section, *nobody* and *has-perm?*. *nobody* is a procedure which yields a
*User*, in this case understood to represent a document with no owner.
*has-perm?* is a procedure which will return a Boolean value indicating whether
a user has permission to check out a document or not; in this specification
we've left it at that. In a different document or if we were also interested in
the specifics of who can check out a document and when, we could continue the
specification with statements about *has-perm?*.

## Installing Pantagruel

The Pantagruel checker is written in [Janet][]. To build from source, make sure
that Janet and `jpm` are installed, and then run:

```
⊕ jpm --local deps
... Dependencies are installed ...
⊕ jpm --local build
... The `pant` binary is compiled and put into `./build`
```

You can then copy the resulting binary into your path. 

## Using the Pantagruel document checker

The above document is available at `priv/checkout.pant`. We can check the
document like this:

```
⊕ pant ./priv/checkout.pant 
⊕
```

The document checker runs on the document and, seeing no errors, exits with
status code 0.

### Checking bindings

The first feature that `pant` exposes is the ability to check that all terms
used in a document have been *bound*, or *defined*, appropriately. 

This is a feature common to nearly every programming language; if a variable
isn't bound, then referring to it should be an error that will crash the
program. However, because Pantagruel documents aren't programs, the binding
rules are slightly more lenient.

In Pantagruel documents, *we may refer to terms we haven't defined yet*.
However, *we must define them by the end of the **next** document section.*

We can see an example of this in the `CHECKOUT` module. In 6, we refer to
`nobody`, even though that word hasn't been defined yet. It's in the *next*
section, in 7, that we actually define it. 

This allows us to write documents in an *explanatory* manner, progressively
introducing and defining terms. This is more useful to humans than a mode where
every term is necessarily defined before it's used, without any useful context.

`pant` will check that we have defined all our terms. To see this in action, we
can remove 7 entirely and re-run the document checker.

```
⊕ pant priv/checkout.pant 
Unglossed symbols:

nobody, has_perm?
```

The document checker has identified the symbols that we failed to define in
time and exits with a non-zero status code.

### Checking types

The second sort of check performed by the document checker is *type checking*.

Just as with binding checks, this is a feature present in many programming
languages. Statically-typed languages can analyze their programs without
running them and assert that they are *well-typed*: for instance, that a
function is only ever called with arguments that match its declared argument
types, or that operators are only ever called on operands with compatible types. 

While Pantagruel's type checking works similarly, it's important to remember
that Pantagruel documents aren't programs, and the purpose of type-checking
them is to encourage rigorous thinking-through---not to avoid illegal program
states that might cause a crash. 

Thus, Pantagruel attempts to alert the user to any obvious violations of the
stated domains of their functions and variables, while still allowing for a
wide degree of expression. 

To see the type checker in action, we can make another change to the example
document. Let's change the first expression in 6 to have a somewhat
nonsensical expression in it, changing our first usage of `nobody` to the
number `0`.

```
owner d = 0 and has_perm? u d -> owner d' = u.
```

Then run the document checker:

```
~/code-src/pantagruel [types!] ⊕ pant priv/checkout.pant
Type error: Couldn't unify types:
User, Nat0
In expression:
...
```

The document checker identifies a type error: the procedure `owner` is
supposed to yield a result in the domain `User`; however, it encountered a
value in the domain `Nat0`---the natural numbers, including
0---instead.[^inexpr]

[^inexpr]: It's worth acknowledging that, currently, the expression being
  evaluated is represented as the raw representation of its AST. This will be
  improved to make it easier to locate type errors in our documents!

Pantagruel's type system, while quite simple by programming language standards,
has some details worth understanding; see the [language reference](./priv/reference.md) for the full
details.

## Background

I had the idea for this language after reading into some of the offerings
currently available in the world of [Formal Methods][FM]: things like [Z],
[TLA+], and somewhat more obscurely, the language developed in [A Practical
Theory of Programming][practheo]. I had and have an interest in formal methods
though my interest, like that of many, is unconsummated, as I haven't become
anything close to expert in any of them and haven't had the opportunity to use
them in a professional setting.

[FM]: https://users.ece.cmu.edu/~koopman/des_s99/formal_methods/
[Z]: https://staff.washington.edu/jon/z-book/index.html
[TLA+]: http://lamport.azurewebsites.net/tla/tla.html
[practheo]: http://www.cs.toronto.edu/~hehner/aPToP/

The desire to put together something of my own came after reading *aPToP*.
Hehner's rationale for formal methods is, paraphrased, is: programming would be
better if program specifications had two closely related qualities that they
currently lack:

1. Unambiguous in meaning
2. Provably correct

To that end he puts together a language of program behavior which is as
unambiguous in its meaning as mathematics with a set of axiomatic equalities
that allow the programmer to express a precise and complete specification of
behavior and then refine it, axiomatically, to arrive at a provably correct
implementation.

I'll leave the reader to decide for themselves whether Hehner's offering is
practicable, but I found myself inspired. I lack the brainpower to write a
formal method that, along the lines of Z and TLA+, allows us to express program
behavior according to some axiomatic theory. So this is not an attempt to
replace or even mimic those systems. However, the notion of a more mathematical
and therefore *less ambiguous* notation for program behavior is appealing. The
observation that English words like *should*, *all*, *none of* and so forth can
contain a multiplicity of overlapping meanings is well-taken.

What would be especially nice is if we had a notation that was more
mathematical but also easily written by hand, and intuitive to the programmer,
so we could use it as a lingua franca for whiteboarding, ideation in notebooks,
and the like. There's a possibility for something with a much lower barrier to
entry (for this admittedly more modest goal) than something like Z. Part of
what makes this possible is that we can jettison a formal *semantics* entirely.
It doesn't need to *mean* anything. At the end of the day the only semantics
will be in the mind of the human reader. So we can try to provide them with a
language that they can use to be as descriptive as is necessary, but only for
their own purposes.

Of course, any language with an undefined semantics will not be *actually*
unambiguous in any objective way. But we might be able to at least avoid
certain classes of less obvious ambiguity.

In other words, it might be nice if we had a system that enforced certain
constraints about exhaustiveness and form while still allowing the author to be
as lazy and hand-wavy as they want if they decide it's not important to be more
specific. There might be a value in forcing ambiguity into sentences like
'There is a procedure *f* (and I won't say anything else about it)' rather than
'When the program is finished, all mailboxes should be empty'.

To that end the last (and only) feature of Pantagruel is that it can be parsed
by a computer program with a well-defined semantics of *binding* and it can
enforce certain constraints on binding. That is, it can force the user to leave
no symbol undefined, even if the definitions entered are nonsensical or wrong.
The hypothesis of Pantagruel is that there is a cognitive benefit to requiring
this kind of exhaustiveness, even if there are no constraints placed on what is
said.
