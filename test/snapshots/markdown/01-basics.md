Basic Pantagruel syntax demonstration
This file shows fundamental language constructs.

### Domains

> Domain declarations define the basic types in your system

`User`.

`Document`.

### Rules

> Rule declarations define operations Syntax: name params => ReturnType.

**owner** *d*: `Document` ⇒ `User`.

> Nullary rules (no parameters)

**nobody** ⇒ `User`.

> Multiple parameters separated by commas

**transfer** *d*: `Document`, *from*: `User`, *to*: `User` ⇒ `Bool`.

---

> Propositions go after the separator (---) They must evaluate to Bool
>
> Simple boolean literals

true.

> Universal quantification

∀ *d*: `Document` · **owner** *d* ∈ `User`.

> Rule application

∀ *d*: `Document`, *u*: `User` · **transfer** *d* *u*
*u* = true.

> Equality and comparison

∀ *u*: `User` · *u* = *u*.

