Advanced features: overrides, qualified names

## Chapter 1

### Domains

`Key`, `Value`

### Rules

**mapping** *k*: `Key` ⇒ `Value`.

**default-value** ⇒ `Value`.

---

> Function override syntax: f[k |-> v] Creates a function like f but with f(k) = v
>
> Override with single mapping

∀ *k*: `Key`, *v*: `Value` · **mapping**[*k* ↦ *v*] *k* = *v*.

> Override preserves other values

∀ *k1*: `Key`, *k2*: `Key`, *v*: `Value` · *k1* ≠ *k2* → **mapping**[*k1* ↦ *v*] *k2* = **mapping** *k2*.

> Override doesn't affect other keys

∀ *k1*: `Key`, *k2*: `Key`, *v*: `Value` · *k1* ≠ *k2* → **mapping**[*k1* ↦ *v*] *k2* = **mapping** *k2*.

## Chapter 2

### Domains

`Item`

### Rules

**available** *i*: `Item` ⇒ `Bool`.

---

∀ *i*: `Item` · **available** *i* ∨ ¬**available** *i*.

> Implication

∀ *i*: `Item` · false → **available** *i*.

> Membership

∀ *i*: `Item` · *i* ∈ `Item`.

