Demonstration of all type features

Built-in types: Bool, Nat, Nat0, Int, Real, String
Nat = positive integers (1, 2, 3, ...)
Nat0 = non-negative integers (0, 1, 2, ...)
Numeric hierarchy: Nat < Nat0 < Int < Real

### Domains

> Domain declarations

`Color`.

`Shape`.

> Type aliases with product types (tuples)

`Point` = `Nat` × `Nat`.

`Point3D` = `Nat` × `Nat` × `Nat`.

`ColoredPoint` = `Point` × `Color`.

> Type aliases with sum types (unions of two inhabited types)

`Tag` = `Color` + `Shape`.

> Type aliases with list types

`Colors` = [`Color`].

`Points` = [`Point`].

`Matrix` = [[`Nat`]].

### Rules

> Rules returning various types

**origin** ⇒ `Point`.

**red** ⇒ `Color`.

**distance** *p*: `Point`, *q*: `Point` ⇒ `Real`.

**colors** ⇒ [`Color`].

> Optional return values: use a list constrained to length <= 1 (Pantagruel has no Maybe/Option type; see guards for the alternative idiom.)

**lookup** *i*: `Nat` ⇒ [`Color`].

---

> Tuple construction with parentheses

**origin** = (0, 0).

> Tuple projection with .N (1-indexed)

**origin**.1 = 0.

**origin**.2 = 0.

> Nested tuple access

∀ *cp*: `ColoredPoint` · *cp*.1.1 ≥ 0.

> List membership

∀ *c*: `Color` · *c* ∈ `Color`.

**red** ∈ **colors**.

> List cardinality

#**colors** ≥ 0.

#`Color` ≥ 0.

> Optional-value idiom: lookup returns at most one element

∀ *i*: `Nat` · #**lookup** *i* ≤ 1.

> Numeric operations respect the hierarchy

∀ *n*: `Nat`, *m*: `Nat0` · *n* + *m* ≥ 0.

∀ *x*: `Int`, *y*: `Real` · *x* + *y* = *y* + *x*.

