# Pantagruel: A Program Specification Language With Unambiguous Syntax and Non-existent Semantics

First and foremost, Pantagruel is tool for *thought* and *communication*. It's
not a programming language, and it's not a program for verifying other
programs. It's a language designed to help you express your ideas about what
programs (or systems, or algorithms, or bylaws...) should do. It is designed to
help you do so in a way that is terser and clearer than natural language;
that's the *communication* part. The *thought* part comes from the bet that
being helped, and sometimes constrained, to communicate in an unambiguous and
slightly more rigorous way than you might otherwise will assist you in thinking
things through.

Pantagruel is a *program specification language*. A notation with a defined
syntax whose sentences will describe the workings of some piece of logic, to be
articulated for the benefit of the people who will implement it, as well as the
people who design it and the people who will ensure it's implemented correctly.

Pantagruel is also implemented in the `pant` interpreter, which is a program
that understands Pantagruel text documents. `pant` is capable of ensuring that
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
owner d : Document => User.

// par. 3
// A specification for a small document management system.

// par. 4
check_out u:User, d:Document.
---

// par. 5
// A user may check out a document if they have permission to access it and
// it's not currently checked out.

// par. 6
((owner d) = nobody and (has_perm? u d)) -> (owner d') = u.
((owner d) != nobody or ~(has_perm? u d)) -> (owner d') = (owner d).

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
