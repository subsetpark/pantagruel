Cond expressions: multi-armed conditionals where each arm is a Bool guard
and each consequence has the same type. Arms are checked for exhaustiveness
during --check.

## Chapter 1

### Domains

`Priority`

### Rules

**score** *p*: `Priority` ⇒ `Nat`.

**tier** *p*: `Priority` ⇒ `Nat`.

---

> Classify priorities into tiers using cond.

∀ *p*: `Priority` · **tier** *p* = (cond **score** *p* ≥ 100 ⇒ 3, **score** *p* ≥ 50 ⇒ 2, **score** *p* ≥ 10 ⇒ 1, true ⇒ 0).

## Chapter 2

### Domains

> A cond used in a simple proposition.

`Status`.

### Rules

**active** *s*: `Status` ⇒ `Bool`.

**rank** *s*: `Status` ⇒ `Nat`.

---

> Compute effective rank: active items use their rank, inactive get 0.

∀ *s*: `Status` · cond **active** *s* ⇒ **rank** *s* ≥ 0, ¬**active** *s* ⇒ **rank** *s* = 0.

