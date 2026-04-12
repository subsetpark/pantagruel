// General function calls: free calls, method calls, zero-arity, nested, spread

declare function max(a: number, b: number): number;
declare function now(): number;
declare function clamp(x: number, lo: number, hi: number): number;

interface Account {
  balance: number;
  owner: string;
}

declare function validate(a: Account): boolean;

/** free function call with two args */
export function freeCall(a: number, b: number): number {
  return max(a, b);
}

/** zero-arity call (no arguments) */
export function zeroArityCall(): number {
  return now();
}

/** nested calls: clamp(x, 0, max(a, b)) */
export function nestedCalls(x: number, a: number, b: number): number {
  return clamp(x, 0, max(a, b));
}

/** call result used in arithmetic */
export function callInArithmetic(a: number, b: number): number {
  return max(a, b) + 1;
}

/** call with property access argument */
export function callWithPropArg(a: Account): boolean {
  return validate(a);
}

/** method call: receiver.method(args) */
export function methodCall(s: string): string {
  return s.toUpperCase();
}

/** call result used in comparison */
export function callInComparison(a: number, b: number): boolean {
  return max(a, b) > 0;
}

/** spread argument in call (should be rejected) */
export function spreadCall(args: number[]): number {
  return max(...args);
}
