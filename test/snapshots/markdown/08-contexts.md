### Contexts

**`Accounts`**

## Chapter 1

### Domains

`Account`, `User`

### Rules

{**`Accounts`**} **balance** *a*: `Account` ⇒ `Nat`.

{**`Accounts`**} **owner** *a*: `Account` ⇒ `User`.

**nobody** ⇒ `User`.

---

> Every account has a positive balance.

∀ *a*: `Account` · **balance** *a* ≥ 0.

## Chapter 2

### Action

> Withdrawal reduces the balance of the target account.

**`Accounts`** ↝ Withdraw *a*: `Account`, *amount*: `Nat`, **balance** *a* ≥ *amount*.

---

**balance**′ *a* = **balance** *a* - *amount*.

∀ *b*: `Account` · *b* ≠ *a* → **balance**′ *b* = **balance** *b*.

∀ *b*: `Account` · **owner**′ *b* = **owner** *b*.

## Chapter 3

### Action

> Transfer moves funds between accounts.

**`Accounts`** ↝ Transfer *from*: `Account`, *to*: `Account`, *amount*: `Nat`, **balance** *from* ≥ *amount*.

---

**balance**′ *from* = **balance** *from* - *amount*.

**balance**′ *to* = **balance** *to* + *amount*.

∀ *b*: `Account` · *b* ≠ *from* ∧ *b* ≠ *to* → **balance**′ *b* = **balance** *b*.

∀ *b*: `Account` · **owner**′ *b* = **owner** *b*.

