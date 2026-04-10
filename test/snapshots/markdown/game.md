## Chapter 1

### Domains

`Player`

### Rules

**finished** ⇒ `Bool`.

**winner** ⇒ `Player`.

**score** *p*: `Player` ⇒ `Nat0`.

---

**finished** → (∀ *q*: `Player`, *q* ≠ **winner** · **score** **winner** > **score** *q*).

initially ¬**finished**.

initially ∀ *p*: `Player` · **score** *p* = 0.

## Chapter 2

### Action

↝ End-Game.

---

¬**finished** → **finished**′.

