# ts2pant

Translate TypeScript functions into checkable [Pantagruel](https://github.com/subsetpark/pantagruel) specifications.

ts2pant reads a TypeScript source file, extracts a function's type signature and body, and emits a `.pant` specification that can be type-checked and verified with bounded model checking via `pant --check`.

## Usage

```sh
ts2pant <file> <function> [options]
```

**Options:**

| Flag | Description |
|------|-------------|
| `--check` | Write a temp `.pant` file and run `pant --check` (requires z3) |
| `--no-body` | Emit declarations only, skip body translation |
| `--numeric-type <type>` | Map `number` to `Int` (default), `Real`, or `Nat0` |

**Examples:**

```sh
# Emit specification to stdout
ts2pant src/account.ts deposit

# Verify the specification with the SMT solver
ts2pant src/account.ts deposit --check
```

## What it translates

### Pure functions

A function that returns a value without mutation is translated as a Pantagruel rule with a body proposition equating the rule application to the return expression.

```typescript
function larger(a: number, b: number): number {
  if (a >= b) {
    return a;
  } else {
    return b;
  }
}
```

```text
module Larger.

larger a: Int, b: Int => Int.

---

all a: Int, b: Int | larger a b = cond a >= b => a, true => b.
```

Supported return expressions: arithmetic, comparisons, ternaries (`? :` becomes `cond`), if/else with returns in both branches.

### Mutating functions

A `void` function with property assignments is translated as a Pantagruel action. Assignments like `account.balance = expr` become primed propositions (`balance' account = expr`).

```typescript
function deposit(account: Account, amount: number): void {
  if (amount <= 0) {
    throw new Error("Amount must be positive");
  }
  account.balance = account.balance + amount;
}
```

```text
module Deposit.

Account.
balance a: Account => Int.
~> Deposit @ account: Account, amount: Int, ~(amount <= 0).

---

balance' account = balance account + amount.
```

### Guard extraction

Preconditions are extracted from three patterns and emitted as declaration guards:

**If-throw patterns:**

```typescript
if (amount <= 0) { throw new Error("..."); }
// becomes: ~(amount <= 0)

if (isValid(x)) { ... } else { throw new Error("..."); }
// becomes: isValid x
```

**Assertion calls** (functions with `asserts` return type):

```typescript
function assert(condition: unknown): asserts condition {
  if (!condition) throw new Error("Assertion failed");
}

function deposit(account: Account, amount: number): void {
  assert(amount > 0);
  assert(account.balance >= 0);
  account.balance = account.balance + amount;
}
```

```text
~> Deposit @ account: Account, amount: Int, amount > 0, balance account >= 0.
```

This works with any function declared as `asserts condition` -- Node's `assert`, `tiny-invariant`, custom assertion helpers, etc. Guard extraction also follows direct, imported, and const-bound helper calls: if a helper function's body consists of leading guard statements (if-throw or assertion calls), those guards are propagated through the call graph with argument substitution.

### Type translation

| TypeScript | Pantagruel |
|-----------|------------|
| `number` | `Int` (default), `Real`, or `Nat0` via `--numeric-type` |
| `boolean` | `Bool` |
| `string` | `String` |
| `T[]` | `[T]` |
| `Set<T>` | `[T]` (membership via `.has(x)` → `x in`, cardinality via `.size` → `#`; uniqueness is not tracked) |
| `Map<K, V>` field on `interface` | two rules: `<name>Key c: C, k: K => Bool` and `<name> c: C, k: K, <name>Key c k => V` (read via `.get(k)` → `<name> c k`, membership via `.has(k)` → `<name>Key c k`) |
| `Map<K, V>` in any other type position (parameter, return, nested) | synthesizes a handle domain per `(K, V)` pair per module: `KToVMap.` plus the same rule pair as above, with the user's interface replaced by the synthesized domain. Non-identifier `K`/`V` are mangled — `[String]` → `ListString`, `A + B` → `AOrB`, `A * B` → `AAndB`. `.get(k)` → `kToVMap m k`, `.has(k)` → `kToVMapKey m k`. Nested Maps register bottom-up (e.g., `Map<string, Map<string, number>>` emits `StringToIntMap` then `StringToStringToIntMapMap`). |
| `T \| null` / `T \| undefined` | `T + Nothing` |
| `interface Foo { ... }` | `Foo.` (domain) + rules for each property |

### Annotations and entailment checking

JSDoc `@pant` annotations specify properties that should be verified as entailed by the translated body. They are emitted into a `check` block:

```typescript
/**
 * @pant all a: Int, b: Int | larger a b >= a and larger a b >= b
 */
function larger(a: number, b: number): number { ... }
```

```text
---

all a: Int, b: Int | larger a b = cond a >= b => a, true => b.

check

all a: Int, b: Int | larger a b >= a and larger a b >= b.
```

With `--check`, the SMT solver verifies that the annotation is *entailed* -- not just satisfiable. For pure functions this means the body propositions entail the annotation; for actions the query also assumes current-state invariants, action preconditions, and frame conditions before negating the goal:

```console
$ ts2pant max.ts larger --check
OK: Invariants are jointly satisfiable
OK: Cond arms are exhaustive: cond a >= b => a, true => b
OK: Entailed: all a: Int, b: Int | larger a b >= a and larger a b >= b
```

If an annotation is not entailed, you get a counterexample:

```console
FAIL: Not entailed: 'balance' account >= 0'
  Before:
    balance account = -1
  Action:
    account = Account_1, amount = 1
  After:
    balance' account = 0
```

## Limitations

- **Single function at a time.** Each invocation translates one function.
- **Limited inter-procedural analysis.** Direct, imported, and const-bound helper calls are followed to extract leading guards. Method calls (`obj.validate(x)`) and dynamically dispatched calls are not followed.
- **No local variables.** Functions with `let`/`const` bindings before the return are rejected as unsupported.
- **No loops.** `for`/`while` are not translated.
- **Array operations** are partially supported: `.filter().map()` chains, `.includes()`, `.length`. Other array methods are unsupported.
- **`Map<K, V>` support is read-only.** Interface-field Maps and Maps in any other type position (parameter, return, nested) both translate into a value rule guarded by a membership predicate — the only difference is whether the owner domain is the user's interface or a synthesized `KToVMap` handle. Mutation (`.set`, `.delete`), construction (`new Map()`), and iteration (`.entries`, `.keys`, `.values`, `.forEach`) are not yet supported.

## Development

```sh
npm install
npm run build
npm test
```

Tests use [Vitest](https://vitest.dev). Fixtures are in `tests/fixtures/`. Snapshot tests capture the full emitted `.pant` output for each fixture.
