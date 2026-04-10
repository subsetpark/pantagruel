A library management system specification
Demonstrates domains, procedures, and state transitions

### Domains

> Domains

`User`.

`Book`.

`Loan`.

### Rules

> Rules with return types

**available** *b*: `Book` ⇒ `Bool`.

**borrower** *b*: `Book` ⇒ `User`.

**loans** *u*: `User` ⇒ [`Loan`].

**book-of** *l*: `Loan` ⇒ `Book`.

**loan-holder** *l*: `Loan` ⇒ `User`.

### Action

> Action for state transitions

↝ Borrow *u*: `User`, *b*: `Book`.

---

> Available books have no borrower constraint Unavailable books have a borrower

∀ *b*: `Book` · ¬**available** *b* → **borrower** *b* ∈ `User`.

> Each loan references a valid book and user

∀ *l*: `Loan` · **book-of** *l* ∈ `Book`.

∀ *l*: `Loan` · **loan-holder** *l* ∈ `User`.

> After borrowing, the book is no longer available

**available**′ *b* = false.

> After borrowing, the borrower is recorded

**borrower**′ *b* = *u*.

