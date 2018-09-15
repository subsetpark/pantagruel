# Pantagruel: An Unambiguous, Undefined Program Specification Language

## What is this?

I had the idea for this language after reading into some of the offerings currently available in the world of [Formal Methods][FM]: things like [Z], [TLA+], and somewhat more obscurely, the language developed in [A Practical Theory of Programming][practheo]. I had and have an interest in formal methods though my interest, like that of many, is unconsummated, as I haven't become anything close to expert in any of them and haven't had the opportunity to use them in a professional setting.

[FM]: https://users.ece.cmu.edu/~koopman/des_s99/formal_methods/
[Z]: https://staff.washington.edu/jon/z-book/index.html
[TLA+]: http://lamport.azurewebsites.net/tla/tla.html
[practho]: http://www.cs.toronto.edu/~hehner/aPToP/

The desire to put together something of my own came after reading aPToP. Hehner's rationale for formal methods is, paraphrased, is: programming would be better if program specifications had two closely related qualities that they currently lack:

1. Unambiguous in meaning
2. Provably correct

To that end he puts together a language of program behavior which is as unambiguous in its meaning as mathematics with a set of axiomatic equalities that allow the programmer to express a precise and complete specification of behavior and then refine it, axiomatically, to arrive at a provably correct implementation.

I'll leave the reader to decide for themselves whether Hehner's offering is practicable, but I found myself inspired. I lack the brainpower to write a formal method that, along the lines of Z and TLA+, allows us to express program behavior according to some axiomatic theory. So this is not an attempt to replace or even mimic those systems. However, the notion of a more mathematical and therefore *less ambiguous* notation for program behavior is appealing. The observation that English words like *should*, *all*, *none of* and so forth can contain a multiplicity of overlapping meanings is well-taken.

What would be especially nice is if we had a notation that was more mathematical but also easily written by hand, and intuitive to the programmer, so we could use it as a lingua franca for whiteboarding, ideation in notebooks, and the like. There's a possibility for something with a much lower barrier to entry (for this admittedly more modest goal) than something like Z. Part of what makes this possible is that we can jettison a formal *semantics* entirely. Because this is a specification language, and because it is not going to result in a provably correct program, it doesn't need to *mean* anything. At the end of the day the only semantics will be in the mind of the human reader. So we can try to provide them with a language that they can use to be as descriptive as is necessary, but only for their own purposes.

Of course, any language with an undefined semantics will not be *actually* unambiguous in any objective way. But we might be able to at least avoid certain classes of less obvious ambiguity.

In other words, it might be nice if we had a system that enforced certain constraints about exhaustiveness and form while still allowing the author to be as lazy and hand-wavy as they want if they decide it's not important to be more specific. There might be a value in forcing ambiguity into sentences like 'There is a function *F*' (and I won't say anything else about it) rather than 'When the program is finished, all mailboxes should be empty'.

To that end the last (and only) feature of Pantagruel is that it can be parsed by a computer program with a well-defined semantics of *binding* and it can enforce certain constraints on binding. That is, it can force the user to leave no symbol undefined, even if the definitions entered are nonsensical or wrong. The hypothesis of Pantagruel is that there is a cognitive benefit to requiring this kind of exhaustiveness, even if there are no constraints placed on what is said.

## A sample program

Here's a trivial but complete Pantagruel program.

```
fib |x : Nat| :: Nat
; A specification for the fibonacci function.
fib x <- fib x - 1 + fib x - 2
fib 1 <- 1
fib 2 <- 1
```

pretty printed, that's:

fib : |x:ℕ| ∷ ℕ

A specification for the fibonacci function.

fib x ← fib x - 1 + fib x - 2
fib 1 ← 1
fib 2 ← 1

Line 1: A function declaration, introducing the `fib` function, which takes some `x` in the domain of the natural numbers and returns some natural number.
Line 2: A comment describing the program.
Line 3: `fib x`, that is, `fib` of any `x`, is *refined* by `fib` of `x - 1` plus `fib` of `x - 2`.
Lines 4,5: `fib` of 1 is 1 and `fib` of 2 is 1.

There are a few components to this, none of which should be particularly novel: line 1 introduces a function and describes the arguments it takes, giving each one a name in the process, in terms of their domains. It also describes the codomain of the function.[^1] Line 2 is an English comment. Any text between a `;` and a newline is a comment; there are no block comments, but multiple lines starting with `;` will be treated as a single paragraph. Lines 3-5 offer *refinements* for `fib`; that is, some *stronger* restatement of a function call `fib x`. In this trivial case, the three statements together completely describe the fibonacci function; in a more complex example, there may be multiple levels of refinement as an idea is fleshed out, which might end up in stubs or lacunae rather than executable code. For instance, we might assert that `f x <- g x 1` and later provide a sketch of the behavior of `g` - for instance, saying that it accepts two integers and returns one - without actually saying what it *does*. A final thing that we can do being that this is not an executable computer program is order the patterns in lines 3-5 according to how we might think of them, with the general case first, rather than how they would need to be in an ML-style pattern match, whence this is obviously stolen, where the base case always needs to go first.

[^1]: Pantagruel uses the words "domain" and "codomain" as oppose to "type" because the natural numbers (here defined as the integers greater than 0) are not a type; they're a set. In Pantagruel the expression `x : X` says that `x` is *in the domain* `X`, but X can be a set of specific values, a generic data type, or anything in between.

## Installation

If [available in Hex](https://hex.pm/docs/publish), the package can be installed
by adding `pantagruel` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:pantagruel, "~> 0.1.0"}
  ]
end
```

Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm). Once published, the docs can
be found at [https://hexdocs.pm/pantagruel](https://hexdocs.pm/pantagruel).

