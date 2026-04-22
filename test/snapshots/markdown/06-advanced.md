Advanced features: overrides, qualified names

## Chapter 1

### Domains

`Key`.

`Value`.

> N-ary override: map-valued rule keyed by an owner handle and a key. Override key is a tuple whose arity matches the rule's parameter list.

`Handle`.

### Rules

**mapping** *k*: `Key` ⇒ `Value`.

**default-value** ⇒ `Value`.

**store** *h*: `Handle`, *k*: `Key` ⇒ `Value`.

---

> Function override syntax: f[k |-> v] Creates a function like f but with f(k) = v
>
> Override with single mapping

∀ *k*: `Key`, *v*: `Value` · **mapping**[*k* ↦ *v*] *k* = *v*.

> Override preserves other values

∀ *k1*: `Key`, *k2*: `Key`, *v*: `Value` · *k1* ≠ *k2* → **mapping**[*k1* ↦ *v*] *k2* = **mapping** *k2*.

> Override doesn't affect other keys

∀ *k1*: `Key`, *k2*: `Key`, *v*: `Value` · *k1* ≠ *k2* → **mapping**[*k1* ↦ *v*] *k2* = **mapping** *k2*.

> N-ary override with a tuple key: the override binds the combined position (h, k) and leaves every other (h', k') untouched.

∀ *h*: `Handle`, *k*: `Key`, *v*: `Value` · **store**[(*h*, *k*) ↦ *v*] *h* *k* = *v*.

∀ *h1*: `Handle`, *h2*: `Handle`, *k1*: `Key`, *k2*: `Key`, *v*: `Value` · *h1* ≠ *h2* ∨ *k1* ≠ *k2* → **store**[(*h1*, *k1*) ↦ *v*] *h2* *k2* = **store** *h2*
*k2*.

## Chapter 2

### Domains

`Item`

### Rules

**available** *i*: `Item` ⇒ `Bool`.

---

∀ *i*: `Item` · **available** *i* ∨ ¬**available** *i*.

> Negation mixed with implication and conjunction

∀ *i*: `Item` · ¬**available** *i* → false ∧ **available** *i*.

> Implication

∀ *i*: `Item` · false → **available** *i*.

> Membership

∀ *i*: `Item` · *i* ∈ `Item`.

