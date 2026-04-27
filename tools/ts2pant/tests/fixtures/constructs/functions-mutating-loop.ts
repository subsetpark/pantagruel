// Structured loop translation (Meijer et al., FPCA 1991). `for-of` and
// `.forEach` over an array become catamorphisms:
//   - Iterator-writes (Shape A): `all x in arr | p' x = v`
//   - Accumulator folds (Shape B): `p' a = p a OP (comb over each x in arr | f)`
// See CLAUDE.md § Structured Iteration.

interface User {
  active: boolean;
  score: number;
}

interface Account {
  total: number;
  balance: number;
}

interface Fee {
  amount: number;
}

interface Item {
  value: number;
  tagged: boolean;
}

/** Shape A: uniform iterator write */
export function activateAll(users: User[]): void {
  for (const u of users) {
    u.active = true;
  }
}

/** Shape A: conditional iterator write — path-merged via cond per element */
export function activateActive(users: User[]): void {
  for (const u of users) {
    if (u.score > 0) {
      u.active = true;
    }
  }
}

/** Shape B: accumulator sum via += */
export function sumAmounts(a: Account, items: Item[]): void {
  for (const x of items) {
    a.total += x.value;
  }
}

/** Shape B: accumulator deduction via -= */
export function deductFees(a: Account, fees: Fee[]): void {
  for (const f of fees) {
    a.balance -= f.amount;
  }
}

/** Shape B with guard — predicate folds into the comprehension */
export function sumPositive(a: Account, items: Item[]): void {
  for (const x of items) {
    if (x.value > 0) {
      a.total += x.value;
    }
  }
}

/** Shape A + Shape B in the same body — *independent*: the iterator write
 * and the fold read touch disjoint properties, so per-step order is
 * semantically irrelevant. */
export function mixedUpdates(a: Account, items: Item[]): void {
  for (const x of items) {
    x.tagged = true;
    a.total += x.value;
  }
}

/** forEach variant of Shape A */
export function forEachActivate(users: User[]): void {
  users.forEach((u) => {
    u.active = true;
  });
}

/** forEach variant of Shape B */
export function forEachSum(a: Account, items: Item[]): void {
  items.forEach((x) => {
    a.total += x.value;
  });
}

/** Shape B where the accumulator is an anonymous record (not a named
 *  interface). The accessor rule is declared under the synthesized
 *  domain (e.g. `total-rec--total`), so `qualifyFieldAccess` in
 *  `classifyLoopStmt` must receive the synth context — otherwise the
 *  emitted `total' acc = ...` would reference a rule that was never
 *  declared. */
export function sumIntoAnon(acc: { total: number }, items: number[]): void {
  for (const x of items) {
    acc.total += x;
  }
}

/** Iterator-INDEPENDENT guard inside a loop body — the inner
 *  `if (!(scale >= 0)) throw …` is a *duplicate* of the same
 *  precondition asserted before the loop, so stripping the inner copy
 *  is sound (the top-level guard already constrains `scale` for any
 *  items length, including 0). The build pass strips the duplicate
 *  inner guard mirroring what `symbolicExecute` does at top level,
 *  and the loop translates to its Shape B fold. Without guard-
 *  filtering inside loop bodies, the inner ThrowStatement would
 *  surface as an unsupported loop body even though it carries no
 *  semantic content beyond the top-level guard. */
export function sumScaledAssert(
  a: Account,
  items: Item[],
  scale: number,
): void {
  if (!(scale >= 0)) {
    throw new Error("scale must be non-negative");
  }
  for (const x of items) {
    if (!(scale >= 0)) {
      throw new Error("scale must be non-negative");
    }
    a.total += x.value;
  }
}

/** Iterator-DEPENDENT guard inside a loop body — `if (!(x.value >= 0))
 *  throw …` is a per-element precondition over the iterable. Stripping
 *  it would emit an unguarded Shape B fold and silently accept inputs
 *  the source program would reject; lifting it as a quantified action
 *  precondition (`all $N in items | x.value >= 0`) is the correct
 *  shape but out of scope for M3, so the build pass rejects. */
export function sumPositiveAssert(a: Account, items: Item[]): void {
  for (const x of items) {
    if (!(x.value >= 0)) {
      throw new Error("non-negative value required");
    }
    a.total += x.value;
  }
}
