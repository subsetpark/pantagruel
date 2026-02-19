  Propositions that couldn't be formalized at all (left as doc comments):

  1. Membership/subset typing (lines 394-399): "x in xs requires xs: [T] and the left operand to be a subtype of the element type." This needs type destructuring — extracting T
   from ty-list T to then state subtype? (infer x env) T. There's no way to pattern-match on a type constructor's result.
  2. Override arity constraint (line 314): "f[k |-> v] requires f to have arity 1." Same issue — need to destructure ty-func params ret and assert #params = 1.
  3. Priming constraint (line 434): "only rules may be primed, not variables." Requires looking up the primed name in the environment and checking is-rule-entry? on the result
  — doable in principle but requires threading env lookups through.
  4. Guard typing (line 436): "guard expressions must have type Bool." There's no way to quantify over elements of a guard list and check each one's type, because guards are an
   opaque domain with no accessor to extract the underlying expression for type inference.
  5. Variable shadowing (line 438): Requires comparing a newly-bound type against an existing binding in the environment — needs a notion of "the type x had before this
  quantifier rebound it."

  Propositions that were expressible but awkward:

  1. Exactly-one-of-N (lines 68-72, 154-158): Four alternatives requires four disjuncts each with three negations — O(n²) boilerplate. An exclusive or disjoint construct, or
  even just being able to say something like count [is-domain? decl, is-alias? decl, is-rule? decl, is-action? decl] = 1, would collapse this to one line.
  2. At-most-one (lines 79-81): "At most one action per chapter" becomes all d1, d2, is-action? d1, is-action? d2 | d1 = d2. This is the standard trick but unintuitive. A
  filtered count — #{d in head c | is-action? d} <= 1 — would be direct.
  3. Split case analysis for Nat literals (lines 281-284): Two separate propositions with different guards (nat-value e = 0 vs nat-value e >= 1) because there's no conditional
  expression. Something like if ... then ... else ... at the expression level would let this be one proposition.
  4. Quantifiers can't appear after →: The grammar restricts the RHS of implication to implication, not expr, so P -> all x: T | Q doesn't parse. This forced me to flatten
  several propositions (the original draft had nested quantifiers that failed to parse). Allowing expr on the RHS of -> (perhaps parenthesized) would help.

  What language features would help most:

  - List filtering / comprehension (#{x in xs | P x} or [x in xs | P x]): Covers at-most-one, counting, and many other patterns.
  - Type/value destructuring or pattern matching on constructors: Would unlock formalizing membership, override, and priming constraints — anything that needs to inspect the
  structure of an inferred type.
  - Conditional expressions (if P then A else B): Collapses split-case propositions into one.
  - Exclusive-or or one-of-N built-in: Drastically simplifies "exactly one kind" partitioning, which comes up constantly in specifications.
