Demonstration of all operators and expressions

### Domains

`Item`

### Rules

**items** ⇒ [`Item`].

**count** ⇒ `Nat0`.

**price** *i*: `Item` ⇒ `Real`.

**in-stock** *i*: `Item` ⇒ `Bool`.

**discount** ⇒ `Real`.

---

> Logical operators: and, or, ~, -> (implication)

∀ *i*: `Item` · **in-stock** *i* ∨ ¬**in-stock** *i*.

∀ *i*: `Item` · **in-stock** *i* ∧ true → **in-stock** *i*.

∀ *i*: `Item` · false → **in-stock** *i*.

> Comparison operators: =, !=, <, >, <=, >=

∀ *i*: `Item` · **price** *i* = **price** *i*.

∀ *i*: `Item` · **price** *i* ≥ 0.0.

**count** ≥ 0.

**discount** < 1.0.

**discount** > 0.0.

**discount** ≤ 0.5.

**discount** ≠ 0.0.

> Arithmetic operators: +, -, *, /

∀ *i*: `Item` · **price** *i* - **discount** · **price** *i* ≥ 0.0.

**count** + 1 > **count**.

∀ *x*: `Nat`, *y*: `Nat` · *x* · *y* ≥ 0.

> Membership: in

∀ *i*: `Item` · *i* ∈ `Item`.

∀ *i*: `Item` · *i* ∈ **items** → **in-stock** *i*.

> Subset (for lists)

∀ *xs*: [`Item`], *ys*: [`Item`] · *xs* ⊆ *ys* → #*xs* ≤ #*ys*.

> Cardinality: #

#**items** ≥ 0.

#`Item` ≥ 0.

∀ *xs*: [`Item`] · #*xs* ≥ 0.

> Unary minus

∀ *x*: `Int` · -*x* + *x* = 0.

∀ *x*: `Real` · --*x* = *x*.

> Negation

∀ *i*: `Item` · ¬¬**in-stock** *i* = **in-stock** *i*.

> Quantifiers: all, some

∀ *i*: `Item` · true.

∃ *i*: `Item` · **in-stock** *i*.

> Quantifier with guard expressions

∀ *i*: `Item`, **in-stock** *i* · **price** *i* > 0.0.

∃ *i*: `Item`, **price** *i* < 10.0 · **in-stock** *i*.

