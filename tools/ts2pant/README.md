# ts2pant

Translate TypeScript functions into checkable [Pantagruel](https://github.com/subsetpark/pantagruel) specifications.

ts2pant reads a TypeScript source file, extracts a function's type signature and body, and emits a `.pant` specification that can be type-checked and verified with bounded model checking via `pant --check`.

## Usage

```
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

```
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

```
module Deposit.

Account.
balance a: Account => Int.
~> Deposit @ account: Account, amount: Int, ~(amount <= 0).

---

balance' account = balance account + amount.
```

### Guard extraction

Preconditions are extracted from two patterns and emitted as declaration guards:

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

```
~> Deposit @ account: Account, amount: Int, amount > 0, balance account >= 0.
```

This works with any function declared as `asserts condition` -- Node's `assert`, `tiny-invariant`, custom assertion helpers, etc.

### Type translation

| TypeScript | Pantagruel |
|-----------|------------|
| `number` | `Int` (default), `Real`, or `Nat0` via `--numeric-type` |
| `boolean` | `Bool` |
| `string` | `String` |
| `T[]` | `[T]` |
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

```
---

all a: Int, b: Int | larger a b = cond a >= b => a, true => b.

check

all a: Int, b: Int | larger a b >= a and larger a b >= b.
```

With `--check`, the SMT solver verifies that the body propositions *entail* the annotation -- not just that they're satisfiable together:

```
$ ts2pant max.ts larger --check
OK: Invariants are jointly satisfiable
OK: Cond arms are exhaustive: cond a >= b => a, true => b
OK: Entailed: all a: Int, b: Int | larger a b >= a and larger a b >= b
```

If an annotation is not entailed, you get a counterexample:

```
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
- **No inter-procedural analysis** (yet). Called helper functions are opaque -- only inline if-throw patterns and `asserts`-typed calls are recognized as guards.
- **No local variables.** Functions with `let`/`const` bindings before the return are rejected as unsupported.
- **No loops.** `for`/`while` are not translated.
- **Array operations** are partially supported: `.filter().map()` chains, `.includes()`, `.length`. Other array methods are unsupported.

## Development

```sh
npm install
npm run build
npm test
```

Tests use [Vitest](https://vitest.dev). Fixtures are in `tests/fixtures/`. Snapshot tests capture the full emitted `.pant` output for each fixture.
