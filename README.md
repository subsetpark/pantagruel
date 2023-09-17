# Pantagruel: A Most Learned Assistant for Rigorous Thinking

Pantagruel is two things:

- A *syntax* for writing documents in;
- A *program* which can read documents written in the Pantagruel syntax and
  validate that they are correct.

For a Pantagruel document to be *correct* means:

- The document author has *introduced* their vocabulary in a structured way;
- The document author has *used* their vocabulary according to how it was
  introduced.

Ideally, the constraints of syntax and sense mean that common sources of
ambiguity in natural human language will be harder to fall into.

Pantagruel should be well-suited for desrcibing sets of rules, behaviours,
systems, or processes. This includes things like:

- Card games;
- Computer programs;
- Poetic forms;
- Dress codes;
- etc.

## A sample document

Here's a trivial but complete Pantagruel document, translated from an example
from *The Way of Z*. It describes the rules for a simple computer program for
managing the checking-out of documents by users.

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

where

// par. 7
nobody => User.
has_perm? u:User, d:Document => Bool.
---
```

This document consists of two *chapters*. Every Pantagruel chapter has a *head*
and a *body*, separated by a horizontal line. 

In the first chapter head we introduce some vocabulary. Vocabulary is of two
basic kinds: *domains*, which represents sets or types of things; and
*procedures*, which represent behaviours with, or relationships between, things
of a certain type. In the first chapter body we make some statements about that
vocabulary: things that are always true about the concepts we're introducing.

In the second section, introduced by the word
`where`, we gloss any new vocabulary that was used in the first section.

¶1: (optional) Module declaration.

¶2: The introduction of our two domains, *User*s and *Document*s, and a
procedure `owner` that specifies the ownership relation between the two.

¶3: A comment introducing the program.

¶4: The introduction of a procedure, *check-out*, which takes *u*,
a *User*, and *d*, a *Document*.

Between ¶4 and ¶5 there is a horizontal line; this separates the introduction
of terms and the statement of propositions about those terms.

¶5: A comment describing our specification in natural language.

¶6: Two propositions describing the expected state after the
*check-out* procedure. The first says that if the owner of *d* is
*nobody* and *has-perm? u d* is true and *check-out u d* is evaluated,
then the successor to *d* (that is, *d* at some next point in time) will
have an owner of *u*. The second line says if either of the two first
conditions is otherwise, the successor to *d* will have the same owner
as *d* does.

`where` acts as a section separator. 

¶7: Provides a gloss for the two procedures referred to in the previous
section, *nobody* and *has-perm?*. *nobody* is a procedure which yields a
*User*, in this case understood to represent a document with no owner.
*has-perm?* is a procedure which will return a Boolean value indicating whether
a user has permission to check out a document or not; in this specification
we've left it at that. In a different document or if we were also interested in
the specifics of who can check out a document and when, we could continue the
specification with statements about *has-perm?*.

## Installing Pantagruel

### From AUR

On any system that uses the [AUR](https://aur.archlinux.org/) (Arch Linux,
Manjaro, et al), Pantagruel can be installed using `pacman` or an equivalent:

```
⊕  yay -S pantagruel
```

### From source

The Pantagruel checker is written in [Janet][]. To build from source, make sure
that Janet and `jpm` are installed, and then run:

```
⊕  jpm --local deps
... Dependencies are installed ...
⊕  jpm --local build
... The `pant` binary is compiled and put into `./build`
```

[Janet]: https://janet-lang.org/

You can then copy the resulting binary into your path. 

## Using the Pantagruel document checker

Run `pant` to evaluate a Pantagruel document.

The above document is available at `priv/checkout.pant`. We can check the
document like this:

```
⊕  pant ./priv/checkout.pant 
⊕
```

The document checker runs on the document and, seeing no errors, exits with
status code 0.

### Checking that terms have been introduced

The first feature that `pant` exposes is the ability to check that all terms
used in a document have been *bound*, *introduced*, or *glossed*, appropriately. 

This is a feature common to nearly every programming language; if a variable
isn't bound, then referring to it should be an error that will crash the
program. However, because Pantagruel documents aren't programs, the binding
rules are slightly more lenient.

Terms can be used in the body of a Pantagruel section without being properly
introduced; however, they need to be "introduced" in the next section for the
document to be correct.

We can see an example of this in the `CHECKOUT` module. In 6, we refer to
`nobody`, even though that word hasn't been defined yet. It's in the *next*
section, in 7, that we actually gloss the term `nobody`. 

This allows us to write documents in an explanatory manner, progressively
introducing and defining terms. Such a mode is not available in a completely
axiomatic system, that requires us to build up our concepts from the most
atomic elements. At the same time, the presence of some requirements around
introducing and glossing terms ensures a greater degree of rigor than a
completely unstructured document.

The `pant` program will check that we have defined all our terms. To see this
in action, we can remove 7 entirely and re-run the document checker:

```
⊕  pant priv/checkout.pant 
checkout.pant:15: unglossed symbol error: has_perm?
checkout.pant:16: unglossed symbol error: has_perm?
checkout.pant:16: unglossed symbol error: nobody
checkout.pant:15: unglossed symbol error: nobody
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
⊕  pant priv/checkout.pant
checkout.pant:15: type error. couldn't unify types for comparison: User and Nat0

in expression:

owner d = 0 and has_perm? u d -> owner d' = u
```

The document checker identifies a type error: the procedure `owner` is supposed
to yield a result in the domain `User`; however, it encountered a value in the
domain `Nat0` (the natural numbers, including 0) instead.

Pantagruel's type system, while quite simple by programming language standards,
has some details worth understanding; see the [language reference](./priv/reference.md) for the full
details.
