// @archlint.module exempt
// @archlint.exempt-reason test-support

interface Account {
  total: number;
}

/**
 * Bounded while whose property accumulator is updated with a
 * dynamically-typed value. `a.total += extra` is lowered by the generic
 * bare-while mutation path (`buildL1BareWhileMutation` →
 * `buildL1PropertyAssignment`), which synthesizes an `a.total + extra` binary
 * expression with no source file. Under policy "opaque", `extra` (typed
 * `any`) lowers to an OpaqueValue, so that synthetic binop reaches
 * `sourceRefForNode` with an opaque operand — the case that used to
 * dereference an undefined source file. `sourceRefForNode` must instead mint a
 * sound, unique synthetic origin (the totality fallback) so the build
 * completes without crashing.
 */
export function addDynamicWhile(a: Account, n: number, extra: any): void {
  let i = 0;
  while (i < n) {
    a.total += extra;
    i++;
  }
}
