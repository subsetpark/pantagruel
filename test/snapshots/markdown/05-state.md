Demonstration of actions and state transitions
Actions model operations that change state
Primed expressions (f') refer to values after the operation

## Chapter 1

### Domains

`User`, `Account`

### Rules

**balance** *a*: `Account` ⇒ `Int`.

**owner** *a*: `Account` ⇒ `User`.

### Action

> Action: no return type means it modifies state

↝ Deposit *a*: `Account`, *amount*: `Nat`.

---

> In a chapter with an action, you can use primed expressions The parameters of the action are in scope
>
> Balance increases by the deposit amount

**balance**′ *a* = **balance** *a* + *amount*.

> Owner doesn't change

**owner**′ *a* = **owner** *a*.

> Other accounts are unchanged (frame condition)

∀ *other*: `Account` · *other* ≠ *a* → **balance**′ *other* = **balance** *other*.

∀ *other*: `Account` · *other* ≠ *a* → **owner**′ *other* = **owner** *other*.

> Pre-condition: can always deposit

∀ *a*: `Account`, *amt*: `Nat` · true.

## Chapter 2

### Action

> A second chapter for withdrawals

↝ Withdraw *a*: `Account`, *amount*: `Nat`.

---

> Balance decreases by withdrawal amount

**balance**′ *a* = **balance** *a* - *amount*.

> Implicit constraint: balance must support the withdrawal

**balance** *a* ≥ *amount*.

> Frame condition

∀ *other*: `Account` · *other* ≠ *a* → **balance**′ *other* = **balance** *other*.

## Chapter 3

### Action

> A third chapter for transfers between accounts

↝ Transfer *from*: `Account`, *to*: `Account`, *amount*: `Nat`.

---

> Source balance decreases

**balance**′ *from* = **balance** *from* - *amount*.

> Destination balance increases

**balance**′ *to* = **balance** *to* + *amount*.

> Pre-conditions

*from* ≠ *to*.

**balance** *from* ≥ *amount*.

> Frame: other accounts unchanged

∀ *other*: `Account` · *other* ≠ *from* ∧ *other* ≠ *to* → **balance**′ *other* = **balance** *other*.

