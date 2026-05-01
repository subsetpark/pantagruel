// State-aware Map/Set reads inside *branched* mutating bodies.
//
// These shapes flow through the L1 build pass (`buildL1IfMutation` →
// `buildL1EffectCall` → `buildL1SubExpr` → `tryBuildL1PureSubExpression`).
// Pre-fix, the `.has` / `.get` fast-paths in `tryBuildL1PureSubExpression`
// emitted bare `Binop(in, ...)` / `App(rule, [...])` forms that ignored
// staged `MapRuleWriteEntry` / `SetRuleWriteEntry` overrides accumulated
// from earlier statements in the same path. The resulting reads missed
// prior `.add` / `.set` and emitted Pantagruel that referenced only the
// pre-state of the rule.
//
// Post-fix, these fast-paths emit `map-read` / `set-read` L1 forms when
// `ctx.state !== undefined`. The body lower path dispatches those forms
// to `readMapThroughWrites` / `readSetThroughWrites` so prior staged
// writes flow into the read inline (override application for Map values,
// `cond` over equality arms for Set membership). The pure / read-only
// path lowers `map-read` / `set-read` mechanically to the same bare
// shapes the fast-path used to emit, so existing pure fixtures stay
// byte-identical. See issue #168.

interface Tagged {
  tags: Set<string>;
  flag: boolean;
}

interface Cache {
  entries: Map<string, number>;
  hits: number;
}

/**
 * Stage A Set: branched `c.tags.add(x)` followed by branched
 * `c.tags.has(x)`. The inner `if`'s guard is built as a `set-read`
 * (state-aware) and lowers via `readSetThroughWrites` to a cond with
 * an `x = x => true` arm — observing the staged `.add`. Pre-fix this
 * lowered to bare `x in (tagged--tags c)`, missing the add.
 */
export function tagThenCheck(c: Tagged, x: string, gate: boolean): void {
  if (gate) {
    c.tags.add(x);
    if (c.tags.has(x)) {
      c.flag = true;
    }
  }
}

/**
 * Stage A Map: branched `c.entries.set(k, v)` followed by branched
 * `c.entries.has(k)`. The inner guard's `map-read` lowers via
 * `readMapThroughWrites` and observes the prior `.set` through the
 * `entries-key` membership predicate's override list.
 */
export function entrySetThenCheck(
  c: Cache,
  k: string,
  v: number,
  gate: boolean,
): void {
  if (gate) {
    c.entries.set(k, v);
    if (c.entries.has(k)) {
      c.hits = c.hits + 1;
    }
  }
}

/**
 * Stage B Map: read-after-write inside a branch. The second `.set`'s
 * value sub-expression `m.get(k)! + 1` contains a `map-read` that the
 * body lower threads through `readMapThroughWrites` against the
 * current state (which has the first `.set` staged). Pre-fix this
 * silently emitted `stringToIntMap m k + 1` — reading the pre-state
 * rule rather than the just-written `v`.
 */
export function bumpInBranch(
  m: Map<string, number>,
  k: string,
  v: number,
  gate: boolean,
): void {
  if (gate) {
    m.set(k, v);
    m.set(k, m.get(k)! + 1);
  }
}

/**
 * Stage B Map: branched `m.has(k)` after a staged `.set` — the
 * inner-if guard's `map-read` observes the staged write through the
 * synthesized `stringToIntMap-key` override list. Combines the
 * `.has`-membership and `.get`-value paths into one shape: the inner
 * branch's `m.set(k, m.get(k)! + 1)` re-exercises the value-side
 * `map-read` from inside a doubly-nested branch.
 */
export function setIfThenBump(
  m: Map<string, number>,
  k: string,
  v: number,
  gate: boolean,
): void {
  if (gate) {
    m.set(k, v);
    if (m.has(k)) {
      m.set(k, m.get(k)! + 1);
    }
  }
}
